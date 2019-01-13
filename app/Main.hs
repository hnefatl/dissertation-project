{-# Language FlexibleContexts #-}
{-# Language LambdaCase #-}

module Main where

import BasicPrelude
import Control.Monad.Except (Except, ExceptT, runExcept, runExceptT, throwError, liftEither, withExceptT)
import TextShow (TextShow, showt)
import Data.Text (unpack, pack)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M

import NameGenerator (NameGenerator, NameGeneratorT, evalNameGeneratorT, embedNG)
import Logger (LoggerT, evalLoggerT)
import Typechecker.Hardcoded

import Language.Haskell.Syntax (HsModule)
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Preprocessor.Renamer (renameModule)
import Typechecker.Typechecker (evalTypeInferrer, inferModuleWithBuiltins)
import Backend.Deoverload (evalDeoverload, deoverloadModule, deoverloadQuantType)
import qualified Backend.ILA as ILA (evalConverter, toIla)
import qualified Backend.ILAANF as ILAANF (ilaToAnf)
import qualified Backend.ILB as ILB (runConverter, anfToIlb)
import qualified Backend.CodeGen as CodeGen (convert)


main :: IO ()
main = do
    inputFile <- getArgs
    let usageMsg = "Usage: compiler-exe <input file>"
    case inputFile of
        [] -> putStrLn usageMsg
        [f] -> compile $ unpack f
        _ -> putStrLn usageMsg

parse :: FilePath -> ExceptT Text IO HsModule
parse f = do
    s <- readFile f
    case parseModule (unpack s) of
        ParseFailed loc err -> throwError $ "Parse failed at " <> showt loc <> " with error:\n" <> pack err
        ParseOk m -> return m


compile :: FilePath -> IO ()
compile f = evalNameGeneratorT (runExceptT x) 0 >>= \case
    Left err -> putStrLn err
    Right () -> return ()
    where x = do
            m <- embedExceptIOIntoExceptNGIO $ parse f
            renamedModule <- embedExceptNGIntoExceptNGIO $ renameModule m
            (taggedModule, types) <- exceptLoggerNGToExceptNGIO $ evalTypeInferrer $ inferModuleWithBuiltins renamedModule
            deoverloadedModule <- exceptLoggerNGToExceptNGIO $ evalDeoverload (deoverloadModule taggedModule) builtinDictionaries types builtinKinds builtinClasses
            let deoverloadedTypes = map deoverloadQuantType types
            ila <- exceptLoggerNGToExceptNGIO $ ILA.evalConverter (ILA.toIla deoverloadedModule) deoverloadedTypes builtinKinds
            ilaanf <- catchAdd ila $ ILAANF.ilaToAnf ila
            ilb <- catchAdd ilaanf $ embedExceptIntoExceptNGIO $ ILB.runConverter (ILB.anfToIlb ilaanf) (M.keysSet builtinConstructors)
            compiled <- catchAdd ilaanf $ CodeGen.convert "Output" "javaexperiment/" ilb
            lift $ lift $ B.writeFile "Output.class" (encode compiled)

catchAdd :: (TextShow a, Monad m) => a -> ExceptT Text m b -> ExceptT Text m b
catchAdd x = withExceptT (\e -> unlines [e, showt x])

-- Utility functions for converting between various monad transformer stacks...
embedExceptIOIntoExceptNGIO :: ExceptT e IO a -> ExceptT e (NameGeneratorT IO) a
embedExceptIOIntoExceptNGIO x = do
    y <- lift $ lift $ runExceptT x
    either throwError return y

embedExceptNGIntoExceptNGIO :: ExceptT e NameGenerator a -> ExceptT e (NameGeneratorT IO) a
embedExceptNGIntoExceptNGIO x = do
    y <- lift $ embedNG $ runExceptT x
    either throwError return y

embedExceptIntoExceptNGIO :: Except e a -> ExceptT e (NameGeneratorT IO) a
embedExceptIntoExceptNGIO = either throwError return . runExcept

discardLoggerFromExceptLoggerNG :: ExceptT e (LoggerT NameGenerator) a -> ExceptT e NameGenerator a
discardLoggerFromExceptLoggerNG x = do
    y <- lift $ evalLoggerT $ runExceptT x
    liftEither y

exceptLoggerNGToExceptNGIO :: ExceptT e (LoggerT NameGenerator) a -> ExceptT e (NameGeneratorT IO) a
exceptLoggerNGToExceptNGIO = embedExceptNGIntoExceptNGIO . discardLoggerFromExceptLoggerNG