module Typechecker.TypeTaggerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Control.Monad.Except
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple
import Text.Printf

import AlphaEq
import ExtraDefs
import NameGenerator
import Typechecker.Typechecker

makeTest :: String -> String -> TestTree
makeTest input expected = testCase (deline input) $ do
    (inputModule, expectedModule) <- case (parseModule input, parseModule expected) of
        (ParseFailed _ msg, _) -> assertFailure $ "Failed to parse input: " ++ msg
        (_, ParseFailed _ msg) -> assertFailure $ "Failed to parse expected: " ++ msg
        (ParseOk m, ParseOk n) -> return (m, n)
    case evalNameGenerator (runExceptT $ evalTypeInferrer $ inferModuleWithBuiltins inputModule) 0 of
        Left msg -> assertFailure $ "Failed to generate tagged tree: " ++ msg
        Right (inputModule', _) -> assertBool err $ alphaEq inputModule' expectedModule
            where err = printf "Expected:\n%s\nGot:\n%s" (unpack $ pShow expectedModule) (unpack $ pShow inputModule')

test :: TestTree
test = testGroup "Type Tagger"
    [

    ]