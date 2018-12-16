module Preprocessor.RenamerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser

import Control.Monad.Except
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple

import AlphaEq
import ExtraDefs
import NameGenerator
import Preprocessor.Renamer

makeTest :: String -> String -> TestTree
makeTest input expected = testCase (deline input) $ case (,) <$> parseModule input <*> parseModule expected of
    ParseOk (input', expected') ->
        case runExcept renamedInput of
            Right actual -> assertBool (unpack $ pShow state) (alphaEq expected' actual)
            Left err -> assertFailure err
        where (renamedInput, state) = evalNameGenerator (runRenamer $ renameModule input') 0
    ParseFailed loc msg -> assertFailure ("Failed to parse input: " ++ show loc ++ "\n" ++ msg)

test :: TestTree
test = testGroup "Renamer"
    [ makeTest "x = 5" "v0 = 5"
    , makeTest
        "x = 5\ny = x"
        "v0 = 5\nv1 = v0"
    , makeTest
        "x = y\ny = x"
        "v0 = v1\nv1 = v0"
    , makeTest
        "(a, b) = (x, 4)\nx = 1"
        "(v0, v1) = (v2, 4)\nv2 = 1"
    , makeTest
        "(_, a) = 4"
        "(_, v0) = 4"
    , makeTest
        "x@(a, b) = (1, 2)"
        "v2@(v0, v1) = (1, 2)"
    , makeTest
        "x = 2\n[a, 2, c] = [1, x, 3]"
        "v2 = 2\n[v0, 2, v1] = [1, v2, 3]"
    , makeTest
        "_ = let { x = 0 ; y = 1 ; z = 2 } in if x then y else z"
        "_ = let { v0 = 0 ; v1 = 1 ; v2 = 2 } in if v0 then v1 else v2"
    , makeTest
        "_ = \\x -> x"
        "_ = \\v0 -> v0"
    , makeTest
        "_ = let { x = 0 ; y = (\\x -> x) } in y"
        "_ = let { v0 = 0 ; v1 = (\\v2 -> v2) } in v1"
    ]