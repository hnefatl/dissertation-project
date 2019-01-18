{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}

module Main where

import BasicPrelude
import System.Exit
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import TextShow (TextShow, showt)
import Data.Text (unpack, pack)
import Data.Default (Default, def)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import NameGenerator (NameGenerator, NameGeneratorT, evalNameGeneratorT, embedNG)
import Logger (LoggerT, runLoggerT, writeLogs, writeLog)
import ExtraDefs (pretty, synPrint)
import Typechecker.Hardcoded

import Language.Haskell.Syntax (HsModule)
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Preprocessor.Renamer (evalRenamer, renameModule)
import Typechecker.Typechecker (evalTypeInferrer, inferModuleWithBuiltins)
import Backend.Deoverload (evalDeoverload, deoverloadModule, deoverloadQuantType)
import qualified Backend.ILA as ILA (runConverter, toIla)
import qualified Backend.ILAANF as ILAANF (ilaToAnf)
import qualified Backend.ILB as ILB (runConverter, anfToIlb)
import qualified Backend.CodeGen as CodeGen (convert, writeClass)

import qualified Data.Set as S
import Typechecker.Types
import Backend.ILA

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
        [] -> putStrLn usageMsg
        [f] -> compile flags $ unpack f
        _ -> putStrLn usageMsg

parse :: FilePath -> ExceptT Text IO HsModule
parse f = do
    s <- readFile f
    case parseModule (unpack s) of
        ParseFailed loc err -> throwError $ "Parse failed at " <> showt loc <> " with error:\n" <> pack err
        ParseOk m -> return m

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
            (renamedModule, topLevelRenames) <- embedExceptNGIntoResult $ evalRenamer $ renameModule m
            (taggedModule, types) <- embedExceptLoggerNGIntoResult $ evalTypeInferrer $ inferModuleWithBuiltins renamedModule
            deoverloadedModule <- embedExceptLoggerNGIntoResult $ evalDeoverload (deoverloadModule taggedModule) builtinDictionaries types builtinKinds builtinClasses
            when (verbose flags) $ writeLog $ unlines ["Deoverloaded", synPrint deoverloadedModule]
            let deoverloadedTypes = map deoverloadQuantType types
            (ila, ilaState) <- embedExceptLoggerNGIntoResult $ ILA.runConverter (ILA.toIla deoverloadedModule) deoverloadedTypes builtinKinds
            when (verbose flags) $ writeLog $ unlines ["ILA", pretty ila]
            ilaanf <- catchAdd ila $ ILAANF.ilaToAnf ila
            when (verbose flags) $ writeLog $ unlines ["ILAANF", pretty ilaanf]
            ilb <- catchAdd ilaanf $ embedExceptIntoResult $ ILB.runConverter (ILB.anfToIlb ilaanf) (M.keysSet builtinConstructors)
            when (verbose flags) $ writeLog $ unlines ["ILB", pretty ilb]
            compiled <- catchAdd ilaanf $ CodeGen.convert "Output" "javaexperiment/" ilb topLevelRenames (datatypes ilaState)
            let outputDir = "out"
            lift $ lift $ lift $ mapM_ (CodeGen.writeClass outputDir) compiled

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