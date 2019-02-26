{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Compiler where

import           BasicPrelude
import           Control.Monad.Except    (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import           Data.Default            (Default, def)
import qualified Data.Map                as M
import           Data.Text               (pack, unpack)
import           System.Exit             (exitFailure)
import           System.FilePath.Glob    as Glob (compile, globDir1)
import           System.Process          (callProcess)
import           TextShow                (TextShow, showt)

import           ExtraDefs               (inverseMap, pretty, synPrint)
import           Logger                  (LoggerT, runLoggerT, writeLog, writeLogs)
import           NameGenerator           (NameGenerator, NameGeneratorT, embedNG, evalNameGeneratorT)
import           Names                   (VariableName)

import qualified Backend.CodeGen         as CodeGen (convert, writeClass)
import           Backend.Deoverload      (deoverloadModule, deoverloadQuantType, evalDeoverload)
import qualified Backend.ILA             as ILA (datatypes, reverseRenamings, runConverter, toIla)
import qualified Backend.ILAANF          as ILAANF (ilaToAnf)
import qualified Backend.ILB             as ILB (anfToIlb)
import           Language.Haskell.Parser (ParseResult(..), parseModule)
import           Language.Haskell.Syntax (HsModule)
import           Preprocessor.Renamer    (evalRenamer, renameModule)
import           Preprocessor.Info       (getModuleKinds)
import           Typechecker.Typechecker (evalTypeInferrer, getClassEnvironment, inferModule)
import Optimisations.LetLifting (performLetLift)
import Optimisations.TopLevelDedupe (performTopLevelDedupe)

data Flags = Flags
    { verbose         :: Bool
    , letLift         :: Bool
    , topLevelDedupe  :: Bool
    , outputDir       :: FilePath
    , outputJar       :: FilePath
    , outputClassName :: FilePath
    , runtimeFileDir  :: FilePath
    , inputFiles      :: [FilePath] }
    deriving (Eq, Show)
instance Default Flags where
    def = Flags
        { verbose = False
        , letLift = True
        , topLevelDedupe = True
        , outputDir = "out"
        , outputJar = "a.jar"
        , outputClassName = "Output"
        , runtimeFileDir = "runtime"
        , inputFiles = []
        }


parse :: FilePath -> ExceptT Text IO HsModule
parse f = do
    s <- readFile f
    case parseModule (unpack s) of
        ParseFailed loc err -> throwError $ "Parse failed at " <> showt loc <> " with error:\n" <> pack err
        ParseOk m           -> return m

printLogsIfVerbose :: Flags -> [Text] -> IO ()
printLogsIfVerbose flags = when (verbose flags) . putStrLn . unlines

compile :: Flags -> FilePath -> IO ()
compile flags f = evalNameGeneratorT (runLoggerT $ runExceptT x) 0 >>= \case
    (Right (), logs) -> printLogsIfVerbose flags logs
    (Left err, logs) -> do
        putStrLn $ unlines ["Error", err]
        printLogsIfVerbose flags logs
        exitFailure
    where x :: ExceptT Text (LoggerT (NameGeneratorT IO)) ()
          x = do
            m <- embedExceptIOIntoResult $ parse f
            (renamedModule, topLevelRenames, reverseRenames1) <- embedExceptLoggerNGIntoResult $ evalRenamer $ renameModule m
            let kinds = getModuleKinds renamedModule
            ((taggedModule, types), classEnvironment) <- catchAddText (synPrint renamedModule) $ embedExceptLoggerNGIntoResult $ evalTypeInferrer $ (,) <$> inferModule kinds renamedModule <*> getClassEnvironment
            (deoverloadedModule, types', kinds') <- embedExceptLoggerNGIntoResult $ evalDeoverload (deoverloadModule taggedModule) types kinds classEnvironment
            when (verbose flags) $ writeLog $ unlines ["Deoverloaded", synPrint deoverloadedModule]
            deoverloadedTypes <- mapM deoverloadQuantType types'
            (ila, ilaState) <- embedExceptLoggerNGIntoResult $ ILA.runConverter (ILA.toIla deoverloadedModule) topLevelRenames deoverloadedTypes kinds'
            let reverseRenames2 = ILA.reverseRenamings ilaState
                reverseRenames = combineReverseRenamings reverseRenames2 reverseRenames1
            mainName <- case M.lookup "_main" (inverseMap reverseRenames) of
                Nothing -> throwError "No _main symbol found."
                Just n  -> return n
            when (verbose flags) $ writeLog $ unlines ["ILA", pretty ila, unlines $ map showt ila, ""]
            ilaanf <- catchAddText (unlines $ map showt ila) $ ILAANF.ilaToAnf ila
            when (verbose flags) $ writeLog $ unlines ["ILAANF", pretty ilaanf, unlines $ map showt ilaanf, ""]
            ilb <- catchAddText (unlines $ map showt ilaanf) $ embedExceptIntoResult $ ILB.anfToIlb ilaanf
            when (verbose flags) $ writeLog $ unlines ["ILB", pretty ilb, unlines $ map showt ilb, ""]
            ilb' <- case letLift flags of
                True -> do
                    ilb' <- performLetLift ilb
                    when (verbose flags) $ writeLog $ unlines ["Let-lifting", pretty ilb', unlines $ map showt ilb', ""]
                    return ilb'
                False -> return ilb
            ilb'' <- case topLevelDedupe flags of
                True -> do
                    ilb'' <- performTopLevelDedupe ilb'
                    when (verbose flags) $ writeLog $ unlines ["Top-level dedupe", pretty ilb'', unlines $ map showt ilb'', ""]
                    return ilb''
                False -> return ilb'
            compiled <- catchAddText (unlines $ map showt ilaanf) $ CodeGen.convert (pack $ outputClassName flags) "javaexperiment/" ilb'' mainName reverseRenames topLevelRenames (ILA.datatypes ilaState)
            lift $ lift $ lift $ mapM_ (CodeGen.writeClass $ outputDir flags) compiled
            lift $ lift $ lift $ makeJar flags

makeJar :: Flags -> IO ()
makeJar flags = do
    let getClassFilePaths dir = do
            paths <- globDir1 (Glob.compile "*.class") dir
            -- Drop the leading directory from each path
            return $ concatMap (\f -> ["-C", dir, drop (1 + length dir) f]) paths
    outputFiles <- getClassFilePaths (outputDir flags)
    runtimeFiles <- getClassFilePaths (runtimeFileDir flags)
    let args = ["cfe", outputDir flags </> outputJar flags, outputClassName flags] <> outputFiles <> runtimeFiles
    callProcess "jar" args

-- |"Extend" one map with another: given mappings x->y and y->z, create a mapping (x+y)->(y+z).
combineReverseRenamings :: M.Map VariableName VariableName -> M.Map VariableName VariableName -> M.Map VariableName VariableName
combineReverseRenamings xs ys = M.unions [xToz, xs, ys]
    where xToz = M.fromList $ flip map (M.toList xs) $ \(x,y) -> maybe (x,y) (x,) (M.lookup y ys)

catchAdd :: (TextShow a, Monad m) => a -> ExceptT Text m b -> ExceptT Text m b
catchAdd x = catchAddText (showt x)
catchAddText :: Monad m => Text -> ExceptT Text m b -> ExceptT Text m b
catchAddText x = withExceptT (\e -> unlines [e, x])

-- Utility functions for converting between various monad transformer stacks...
embedExceptIOIntoResult :: ExceptT e IO a -> ExceptT e (LoggerT (NameGeneratorT IO)) a
embedExceptIOIntoResult x = do
    y <- lift $ lift $ lift $ runExceptT x
    either throwError return y

embedExceptNGIntoResult :: ExceptT e NameGenerator a -> ExceptT e (LoggerT (NameGeneratorT IO)) a
embedExceptNGIntoResult x = do
    y <- lift $ lift $ embedNG $ runExceptT x
    either throwError return y

embedExceptLoggerNGIntoResult :: ExceptT e (LoggerT NameGenerator) a -> ExceptT e (LoggerT (NameGeneratorT IO)) a
embedExceptLoggerNGIntoResult x = do
    (y, logs) <- lift $ lift $ embedNG $ runLoggerT $ runExceptT x
    writeLogs logs
    either throwError return y

embedExceptIntoResult :: Except e a -> ExceptT e (LoggerT (NameGeneratorT IO)) a
embedExceptIntoResult = either throwError return . runExcept
