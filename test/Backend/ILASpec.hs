{-# Language FlexibleContexts #-}

module Backend.ILASpec where

import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Control.Monad.Except

import ExtraDefs
import Names
import NameGenerator
import Typechecker.Typechecker
import Backend.ILA

parse :: MonadError String m => String -> m HsModule
parse s = case parseModule s of
    ParseOk m -> return m
    ParseFailed loc msg -> throwError (msg ++ ": " ++ show loc)

makeTest :: String -> [Binding] -> TestTree
makeTest input expected = testCase (deline input) $ do
    case runExcept foo of
        Left err -> assertFailure err
        Right binds -> assertEqual "" binds expected
    where foo = do
            m <- parse input
            let (eTypes, counter) = runNameGenerator (evalTypeInferrer $ inferModuleWithBuiltins m) 0
            types <- eTypes
            evalNameGenerator (runConverter (toIla m) types) counter


test :: TestTree
test = testGroup "Backend"
    [
        makeTest "x = (True, False)" [NonRec (VariableName "x") $ Var $ VariableName "y"]
    ]