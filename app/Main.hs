{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections    #-}

module Main where

import           BasicPrelude
import           Control.Monad.Except    (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import           Data.Default            (Default, def)
import qualified Data.Map                as M
import           Data.Text               (pack, unpack)
import           System.Exit
import           TextShow                (TextShow, showt)

import           ExtraDefs               (pretty, synPrint)
import           Logger                  (LoggerT, runLoggerT, writeLog, writeLogs)
import           NameGenerator           (NameGenerator, NameGeneratorT, embedNG, evalNameGeneratorT)
import           Names                   (VariableName)
import           Typechecker.Hardcoded

import qualified Backend.CodeGen         as CodeGen (convert, writeClass)
import           Backend.Deoverload      (deoverloadModule, deoverloadQuantType, evalDeoverload)
import qualified Backend.ILA             as ILA (datatypes, reverseRenamings, runConverter, toIla)
import qualified Backend.ILAANF          as ILAANF (ilaToAnf)
import qualified Backend.ILB             as ILB (anfToIlb)
import           Language.Haskell.Parser (ParseResult(..), parseModule)
import           Language.Haskell.Syntax (HsModule)
import           Preprocessor.Renamer    (evalRenamer, renameModule)
import           Typechecker.Typechecker (evalTypeInferrer, inferModule)


data Flags = Flags
    { verbose :: Bool }
instance Default Flags where
    def = Flags
        { verbose = True }

main :: IO ()
main = do
    let flags = def
    inputFile <- getArgs
    let usageMsg = "Usage: compiler-exe <input file>"
    case inputFile of
        []  -> putStrLn usageMsg
        [f] -> compile flags $ unpack f
        _   -> putStrLn usageMsg

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
            (renamedModule, reverseRenames1) <- embedExceptNGIntoResult $ evalRenamer $ renameModule m
            mainName <- case M.toList $ M.filter (== "_main") reverseRenames1 of
                []       -> throwError "No _main symbol found."
                [(n, _)] -> return n
                ns       -> throwError $ "Multiple _main symbols found: " <> showt (map fst ns)
            (taggedModule, types) <- embedExceptLoggerNGIntoResult $ evalTypeInferrer $ inferModule renamedModule
            deoverloadedModule <- embedExceptLoggerNGIntoResult $ evalDeoverload (deoverloadModule taggedModule) builtinDictionaries types builtinKinds builtinClasses
            when (verbose flags) $ writeLog $ unlines ["Deoverloaded", synPrint deoverloadedModule]
            let deoverloadedTypes = map deoverloadQuantType types
            (ila, ilaState) <- embedExceptLoggerNGIntoResult $ ILA.runConverter (ILA.toIla deoverloadedModule) deoverloadedTypes builtinKinds
            let reverseRenames2 = ILA.reverseRenamings ilaState
                reverseRenames = combineReverseRenamings reverseRenames2 reverseRenames1
            when (verbose flags) $ writeLog $ unlines ["ILA", pretty ila]
            ilaanf <- catchAdd ila $ ILAANF.ilaToAnf ila
            when (verbose flags) $ writeLog $ unlines ["ILAANF", pretty ilaanf]
            ilb <- catchAdd ilaanf $ embedExceptIntoResult $ ILB.anfToIlb ilaanf
            when (verbose flags) $ writeLog $ unlines ["ILB", pretty ilb]
            compiled <- catchAdd ilaanf $ CodeGen.convert "Output" "javaexperiment/" ilb mainName reverseRenames (ILA.datatypes ilaState)
            let outputDir = "out"
            lift $ lift $ lift $ mapM_ (CodeGen.writeClass outputDir) compiled

-- |"Extend" one map with another: given mappings x->y and y->z, create a mapping (x+y)->(y+z).
combineReverseRenamings :: M.Map VariableName VariableName -> M.Map VariableName VariableName -> M.Map VariableName VariableName
combineReverseRenamings xs ys = M.unions [xToz, xs, ys]
    where xToz = M.fromList $ flip map (M.toList xs) $ \(x,y) -> maybe (x,y) (x,) (M.lookup y ys)

catchAdd :: (TextShow a, Monad m) => a -> ExceptT Text m b -> ExceptT Text m b
catchAdd x = withExceptT (\e -> unlines [e, showt x])

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
