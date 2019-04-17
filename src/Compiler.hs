{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Compiler where

import           BasicPrelude                      hiding (pred)
import           Control.Exception                 (try)
import           Control.Monad.Except              (Except, ExceptT, MonadError, liftEither, runExcept, runExceptT,
                                                    throwError)
import           Data.Default                      (Default, def)
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import           Data.Text                         (pack, unpack)
import           Data.Text.Lazy                    (toStrict)
import           Data.Text.Template                (substitute)
import           System.Directory                  (copyFile, createDirectoryIfMissing, listDirectory)
import           System.Exit                       (exitFailure)
import           System.IO.Temp                    (withSystemTempDirectory)
import           System.Process                    (callProcess, readCreateProcess, shell)
import           TextShow                          (showt)

import           ExtraDefs                         (endsWith, inverseMap, pretty, synPrint)
import           Logger                            (LoggerT, MonadLogger, runLoggerT, writeLog, writeLogs)
import           NameGenerator                     (NameGenerator, NameGeneratorT, embedNG, evalNameGeneratorT)
import           Names                             (VariableName, convertName)
import           StdLibEmbed                       (stdLibContents)

import qualified Backend.CodeGen                   as CodeGen (convert, writeClass)
import           Backend.Deoverload                (deoverloadModule, deoverloadQuantType, runDeoverload)
import qualified Backend.Deoverload                as Deoverloader
import           Backend.ILA                       (Binding(..))
import qualified Backend.ILA                       as ILA (datatypes, reverseRenamings, runConverter, toIla)
import qualified Backend.ILAANF                    as ILAANF (ilaToAnf)
import qualified Backend.ILB                       as ILB (Rhs(..), anfToIlb)
import           Language.Haskell.Parser           (ParseResult(..), parseModule)
import           Language.Haskell.Syntax           (HsModule(..))
import           Optimisations.BindingDedupe       (doDedupe)
import           Optimisations.LetLifting          (performLetLift)
import           Optimisations.UnreachableCodeElim (elimUnreachableCode)
import           Preprocessor.Info                 (getClassInfo, getModuleKinds)
import           Preprocessor.Renamer              (evalRenamer, renameModule)
import           Typechecker.Typechecker           (evalTypeInferrer, getClassEnvironment, inferModule)

data Flags = Flags
    { verbose         :: Bool
    , letLift         :: Bool
    , dedupe          :: Bool
    , unreachableElim :: Bool
    , noStdImport     :: Bool
    , dumpTypes       :: Bool
    , buildDir        :: FilePath
    , outputJar       :: FilePath
    , outputClassName :: FilePath
    , runtimeFileDir  :: FilePath
    , package         :: FilePath
    , inputFiles      :: [FilePath] }
    deriving (Eq, Show)
instance Default Flags where
    def = Flags
        { verbose = False
        , letLift = True
        , dedupe = True
        , unreachableElim = True
        , noStdImport = False
        , dumpTypes = False
        , buildDir = "out"
        , outputJar = "a.jar"
        , outputClassName = "Output"
        , runtimeFileDir = "runtime"
        , package = ""
        , inputFiles = []
        }

parse :: (MonadIO m, MonadError Text m) => FilePath -> m HsModule
parse f = liftIO (try $ readFile f) >>= \case
    Left e -> throwError $ "Failed to read file " <> pack f <> ":\n" <> showt (e :: SomeException)
    Right s -> parseContents (unpack s)

parseContents :: MonadError Text m => String -> m HsModule
parseContents contents = case parseModule contents of
    ParseFailed loc err -> throwError $ "Parse failed at " <> showt loc <> " with error:\n" <> pack err
    ParseOk m           -> return m

printLogsIfVerbose :: Flags -> [Text] -> IO ()
printLogsIfVerbose flags = when (verbose flags) . putStrLn . unlines

makeOptimisation :: MonadLogger m => (Flags -> Bool) -> Text -> ([Binding ILB.Rhs] -> m [Binding ILB.Rhs]) -> Flags -> ([Binding ILB.Rhs] -> m [Binding ILB.Rhs])
makeOptimisation pred name opt flags prog
    | not (pred flags) = return prog -- Optimisation not enabled
    | otherwise = do
        prog' <- opt prog
        writeLog $ unlines [name, pretty prog', unlines $ map showt prog', ""]
        return prog'

compile :: Flags -> FilePath -> IO ()
compile flags f = evalNameGeneratorT (runLoggerT $ runExceptT x) 0 >>= \case
    (Right (), logs) -> printLogsIfVerbose flags logs
    (Left err, logs) -> do
        putStrLn $ unlines ["Error", err]
        printLogsIfVerbose flags logs
        exitFailure
    where
        x :: ExceptT Text (LoggerT (NameGeneratorT IO)) ()
        x = do
            originalModule <- embedExceptIOIntoResult $ parse f
            m <- if noStdImport flags then return originalModule else mergePreludeModule originalModule
            (renamedModule, topLevelRenames, reverseRenames1) <- embedExceptLoggerNGIntoResult $ evalRenamer $ renameModule m
            let kinds = getModuleKinds renamedModule
            moduleClassInfo <- getClassInfo renamedModule
            ((taggedModule, types), classEnvironment) <- embedExceptLoggerNGIntoResult $ evalTypeInferrer $ (,) <$> inferModule kinds moduleClassInfo renamedModule <*> getClassEnvironment
            writeLog ""
            writeLog ""
            writeLog $ showt classEnvironment
            writeLog ""
            writeLog ""
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
            when (dumpTypes flags) $ forM_ (M.toList topLevelRenames) $ \(origName, renamed) ->
                case M.lookup renamed types' of
                    Nothing -> return ()
                    Just t -> putStrLn $ showt origName <> " :: " <> showt t
            mainName <- case M.lookup "main" (inverseMap reverseRenames) of
                Nothing -> throwError "No main symbol found."
                Just n  -> return n
            writeLog $ unlines ["ILA", pretty ila, unlines $ map showt ila, ""]
            ilaanf <- ILAANF.ilaToAnf ila
            writeLog $ unlines ["ILAANF", pretty ilaanf, unlines $ map showt ilaanf, ""]
            ilb <- embedExceptIntoResult $ ILB.anfToIlb ilaanf
            writeLog $ unlines ["ILB", pretty ilb, unlines $ map showt ilb, ""]
            -- Optimisations
            let letLiftOpt = makeOptimisation letLift "Let-lifting" performLetLift flags
                dedupeOpt = makeOptimisation dedupe "Dedupe" doDedupe flags
                unreachOpt = makeOptimisation unreachableElim "Unreachable Code" (pure . elimUnreachableCode mainName) flags
            ilb' <- letLiftOpt ilb >>= dedupeOpt >>= unreachOpt
            compiled <- CodeGen.convert (pack $ outputClassName flags) (pack $ package flags) "javaexperiment/" ilb' mainName reverseRenames topLevelRenames (ILA.datatypes ilaState)
            -- Write the class files to eg. out/<input file name>/*.class, creating the dir if necessary
            let classDir = buildDir flags </> package flags
            lift $ lift $ lift $ liftIO $ createDirectoryIfMissing True classDir
            lift $ lift $ lift $ mapM_ (liftIO . CodeGen.writeClass classDir) compiled
            lift $ compileRuntimeFiles flags
            lift $ lift $ lift $ makeJar flags

mergePreludeModule :: (MonadIO m, MonadError Text m) => HsModule -> m HsModule
mergePreludeModule (HsModule a b c d decls) = do
    HsModule _ _ _ _ decls' <- parseContents stdLibContents
    return $ HsModule a b c d (decls <> decls')

compileRuntimeFiles :: Flags -> LoggerT (NameGeneratorT IO) ()
compileRuntimeFiles flags = do
    -- Find all the runtime source files
    runtimeSrcFiles <- liftIO $ filter (\f -> pack f `endsWith` ".java") <$> listDirectory (runtimeFileDir flags)
    let variableReplacement "package" = pack (package flags)
        variableReplacement v         = error $ "Unknown variable " <> unpack v <> " in runtime source file"
    buildOutput <- liftIO $ withSystemTempDirectory "compiler-runtime" $ \tmpDir -> do
        -- Read the runtime source files, replace the package name, write them to the temporary directory
        forM_ runtimeSrcFiles $ \runtimeFile -> do
            contents <- readFile $ runtimeFileDir flags </> runtimeFile
            let newContents = substitute contents variableReplacement
            writeFile (tmpDir </> runtimeFile) (toStrict newContents)
        -- Compile the runtime source within the temporary directory
        buildOutput <- readCreateProcess (shell $ "javac -Xlint:all " <> (tmpDir </> "*.java")) ""
        -- Copy compiled files to the build directory
        let classDir = buildDir flags </> package flags
        runtimeBinFiles <- filter (\f -> pack f `endsWith` ".class") <$> listDirectory tmpDir
        forM_ runtimeBinFiles $ \file -> liftIO $ copyFile (tmpDir </> file) (classDir </> file)
        return buildOutput
    writeLog "Output from building runtime files:"
    writeLog $ pack buildOutput

makeJar :: Flags -> IO ()
makeJar flags = do
    -- Call `jar` to construct the executable jar file
    let mainClassName = package flags <> "." <> outputClassName flags
        args = ["cfe", outputJar flags, mainClassName, "-C", buildDir flags, "."]
    callProcess "jar" args

-- |"Extend" one map with another: given mappings x->y and y->z, create a mapping (x+y)->(y+z).
combineReverseRenamings :: M.Map VariableName VariableName -> M.Map VariableName VariableName -> M.Map VariableName VariableName
combineReverseRenamings xs ys = M.unions [xToz, xs, ys]
    where xToz = M.fromList $ flip map (M.toList xs) $ \(x,y) -> maybe (x,y) (x,) (M.lookup y ys)

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
