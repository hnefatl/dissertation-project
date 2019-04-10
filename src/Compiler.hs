{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Compiler where

import           BasicPrelude
import           Control.Exception           (try)
import           Control.Monad.Except        (Except, ExceptT, MonadError, liftEither, runExcept, runExceptT,
                                              throwError, withExceptT)
import           Data.Default                (Default, def)
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Text                   (pack, unpack)
import           System.Exit                 (exitFailure)
import           System.FilePath.Glob        as Glob (compile, globDir1)
import           System.Process              (callProcess)
import           TextShow                    (TextShow, showt)

import           ExtraDefs                   (inverseMap, pretty, synPrint)
import           Logger                      (LoggerT, runLoggerT, writeLog, writeLogs)
import           NameGenerator               (NameGenerator, NameGeneratorT, embedNG, evalNameGeneratorT)
import           Names                       (VariableName, convertName)

import qualified Backend.CodeGen             as CodeGen (convert, writeClass)
import           Backend.Deoverload          (deoverloadModule, deoverloadQuantType, runDeoverload)
import qualified Backend.Deoverload          as Deoverloader
import qualified Backend.ILA                 as ILA (datatypes, reverseRenamings, runConverter, toIla)
import qualified Backend.ILAANF              as ILAANF (ilaToAnf)
import qualified Backend.ILB                 as ILB (anfToIlb)
import           Language.Haskell.Parser     (ParseResult(..), parseModule)
import           Language.Haskell.Syntax     (HsModule(..))
import           Optimisations.BindingDedupe (dedupe)
import           Optimisations.LetLifting    (performLetLift)
import           Preprocessor.Info           (getClassInfo, getModuleKinds)
import           Preprocessor.Renamer        (evalRenamer, renameModule)
import           Typechecker.Typechecker     (evalTypeInferrer, getClassEnvironment, inferModule)

data Flags = Flags
    { verbose         :: Bool
    , letLift         :: Bool
    , topLevelDedupe  :: Bool
    , noStdImport     :: Bool
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
        , noStdImport = False
        , outputDir = "out"
        , outputJar = "a.jar"
        , outputClassName = "Output"
        , runtimeFileDir = "runtime"
        , inputFiles = []
        }


parse :: (MonadIO m, MonadError Text m) => FilePath -> m HsModule
parse f = liftIO (try $ readFile f) >>= \case
    Left e -> throwError $ "Failed to read file " <> pack f <> ":\n" <> showt (e :: SomeException)
    Right s -> case parseModule (unpack s) of
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
            originalModule <- embedExceptIOIntoResult $ parse f
            m <- if noStdImport flags then return originalModule else mergePreludeModule originalModule
            (renamedModule, topLevelRenames, reverseRenames1) <- embedExceptLoggerNGIntoResult $ evalRenamer $ renameModule m
            let kinds = getModuleKinds renamedModule
                moduleClassInfo = getClassInfo renamedModule
            ((taggedModule, types), classEnvironment) <- catchAddText (synPrint renamedModule) $ embedExceptLoggerNGIntoResult $ evalTypeInferrer $ (,) <$> inferModule kinds moduleClassInfo renamedModule <*> getClassEnvironment
            writeLog $ unlines ["Tagged module:", synPrint taggedModule]
            (deoverloadResult, deoverloadState) <- embedLoggerNGIntoResult $ runDeoverload (deoverloadModule moduleClassInfo taggedModule) types kinds classEnvironment
            deoverloadedModule <- liftEither $ runExcept deoverloadResult
            let types' = Deoverloader.types deoverloadState
                kinds' = Deoverloader.kinds deoverloadState
                dicts  = Deoverloader.dictionaries deoverloadState
                dictNames = S.map convertName $ M.keysSet moduleClassInfo
            writeLog $ unlines ["Deoverloaded module:", synPrint deoverloadedModule]
            writeLog $ unlines ["Deoverloaded", synPrint deoverloadedModule]
            deoverloadedTypes <- mapM deoverloadQuantType types'
            (ila, ilaState) <- embedExceptLoggerNGIntoResult $ ILA.runConverter (ILA.toIla deoverloadedModule) topLevelRenames deoverloadedTypes kinds' dicts dictNames
            let reverseRenames2 = ILA.reverseRenamings ilaState
                reverseRenames = combineReverseRenamings reverseRenames2 reverseRenames1
            mainName <- case M.lookup "main" (inverseMap reverseRenames) of
                Nothing -> throwError "No main symbol found."
                Just n  -> return n
            writeLog $ unlines ["ILA", pretty ila, unlines $ map showt ila, ""]
            ilaanf <- catchAddText (unlines $ map showt ila) $ ILAANF.ilaToAnf ila
            writeLog $ unlines ["ILAANF", pretty ilaanf, unlines $ map showt ilaanf, ""]
            ilb <- catchAddText (unlines $ map showt ilaanf) $ embedExceptIntoResult $ ILB.anfToIlb ilaanf
            writeLog $ unlines ["ILB", pretty ilb, unlines $ map showt ilb, ""]
            ilb' <- if letLift flags then do
                        ilb' <- performLetLift ilb
                        writeLog $ unlines ["Let-lifting", pretty ilb', unlines $ map showt ilb', ""]
                        return ilb'
                    else return ilb
            ilb'' <- if topLevelDedupe flags then do
                        ilb'' <- dedupe ilb'
                        writeLog $ unlines ["Top-level dedupe", pretty ilb'', unlines $ map showt ilb'', ""]
                        return ilb''
                    else return ilb'
            compiled <- catchAddText (unlines $ map showt ilaanf) $ CodeGen.convert (pack $ outputClassName flags) "javaexperiment/" ilb'' mainName reverseRenames topLevelRenames (ILA.datatypes ilaState)
            lift $ lift $ lift $ mapM_ (CodeGen.writeClass $ outputDir flags) compiled
            lift $ lift $ lift $ makeJar flags

mergePreludeModule :: (MonadIO m, MonadError Text m) => HsModule -> m HsModule
mergePreludeModule (HsModule a b c d decls) = do
    HsModule _ _ _ _ decls' <- parse "StdLib.hs"
    return $ HsModule a b c d (decls <> decls')

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

embedLoggerNGIntoResult :: LoggerT NameGenerator a -> ExceptT e (LoggerT (NameGeneratorT IO)) a
embedLoggerNGIntoResult x = do
    (y, logs) <- lift $ lift $ embedNG $ runLoggerT x
    writeLogs logs
    return y
