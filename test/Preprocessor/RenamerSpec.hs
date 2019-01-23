module Preprocessor.RenamerSpec where

import BasicPrelude
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (Assertion, assertFailure, testCase)

import Language.Haskell.Parser (ParseResult(..), parseModule)

import Control.Monad.Except    (runExcept)
import Data.Text               (pack, unpack)
import TextShow                (showt)

import AlphaEq                 (AlphaEq, alphaEqError)
import ExtraDefs               (deline, pretty, synPrint)
import NameGenerator           (evalNameGenerator)
import Preprocessor.Renamer    (renameModule, runRenamer)

assertAlphaEq :: AlphaEq a => Text -> a -> a -> Assertion
assertAlphaEq msg x y = case runExcept $ alphaEqError x y of
    Left err -> assertFailure $ unpack $ unlines [err, msg]
    Right () -> return ()

makeTest :: Text -> Text -> TestTree
makeTest input expected =
    testCase (unpack $ deline input) $ case (,) <$> parseModule (unpack input) <*> parseModule (unpack expected) of
        ParseOk (input', expected') ->
            case runExcept renamedInput of
                Right (actual, _) ->
                    assertAlphaEq (unlines [showt actual, synPrint actual, synPrint expected', pretty state]) expected' actual
                Left err     -> assertFailure $ unpack $ unlines [err, pretty state]
            where (renamedInput, state) = evalNameGenerator (runRenamer $ renameModule input') 0
        ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg

test :: TestTree
test = testGroup "Renamer"
    [ makeTest "x = 5" "v0 = 5"
    , makeTest
        "x = 5 ; y = x"
        "v0 = 5 ; v1 = v0"
    , makeTest
        "x = 1 + 2"
        "v0 = 1 `v1` 2"
    , makeTest
        "x = y ; y = x"
        "v0 = v1 ; v1 = v0"
    , makeTest
        "(a, b) = (x, 4) ; x = 1"
        "(v0, v1) = (v2, 4) ; v2 = 1"
    , makeTest
        "(_, a) = 4"
        "(_, v0) = 4"
    , makeTest
        "x@(a, b) = (1, 2)"
        "v2@(v0, v1) = (1, 2)"
    , makeTest
        "x = 2 ; [a, 2, c] = [1, x, 3]"
        "v2 = 2 ; [v0, 2, v1] = [1, v2, 3]"
    , makeTest
        "_ = let { x = 0 ; y = 1 ; z = 2 } in if x then y else z"
        "_ = let { v0 = 0 ; v1 = 1 ; v2 = 2 } in if v0 then v1 else v2"
    , makeTest
        "_ = \\x -> x"
        "_ = \\v0 -> v0"
    , makeTest
        "_ = let { x = 0 ; y = (\\x -> x) } in y"
        "_ = let { v0 = 0 ; v1 = (\\v2 -> v2) } in v1"
    , makeTest
        "f = (\\x -> x) :: a -> a ; g = f :: a -> a"
        "f = (\\x -> x) :: t0 -> t0 ; g = f :: t1 -> t1"
    , makeTest
        "class Foo a where { bar :: Int -> a -> a }"
        "class Foo t0 where { v1 :: Int -> t0 -> t0 }"
    , makeTest
        "data Foo a b = Foo a b ; f = \\x (Foo y z@(w, u)) -> (y, u)"
        "data Foo a b = Foo a b ; f = \\v0 (Foo v1 v2@(v3, v4)) -> (v1, v4)"
    , makeTest
        "f x = x"
        "v0 v1 = v1"
    , makeTest
        "data Bool = True | False ; f True x 0 = x"
        "data Bool = True | False ; v0 True v2 0 = v2"
    ]
