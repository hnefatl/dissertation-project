module Typechecker.TypeTaggerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Language.Haskell.Syntax

import AlphaEq
import ExtraDefs
import Typechecker.Types
import Typechecker.Typechecker

makeTest :: String -> String -> TestTree
makeTest untagged tagged = testCase (deline untagged) $ do
    (untaggedModule, taggedModule) <- case (parseModule untagged, parseModule tagged) of
        (ParseFailed _ msg, _) -> assertFailure $ "Failed to parse untagged: " ++ msg
        (_, ParseFailed _ msg) -> assertFailure $ "Failed to parse tagged: " ++ msg
        (ParseOk m, ParseOk n) -> return (m, n)
    undefined
    --assertBool $ alphaEq 

test :: TestTree
test = testGroup "Type Tagger"
    [

    ]