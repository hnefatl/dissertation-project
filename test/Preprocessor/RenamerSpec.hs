module Preprocessor.RenamerSpec where

import BasicPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)

import Language.Haskell.Parser (parseModule, ParseResult(..))

import TextShow (TextShow, showt)
import Control.Monad.Except (runExcept)
import Data.Text (unpack, pack)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pString)

import AlphaEq (alphaEq)
import ExtraDefs (deline)
import NameGenerator (evalNameGenerator)
import Preprocessor.Renamer (runRenamer, renameModule)

pretty :: TextShow a => a -> Text
pretty = toStrict . pString . unpack . showt

makeTest :: Text -> Text -> TestTree
makeTest input expected =
    testCase (unpack $ deline input) $ case (,) <$> parseModule (unpack input) <*> parseModule (unpack expected) of
        ParseOk (input', expected') ->
            case runExcept renamedInput of
                Right actual -> assertBool (unpack $ pretty state) (alphaEq expected' actual)
                Left err -> assertFailure $ unpack $ unlines [err, pretty state]
            where (renamedInput, state) = evalNameGenerator (runRenamer $ renameModule input') 0
        ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg

test :: TestTree
test = testGroup "Renamer"
    [ makeTest "x = 5" "v0 = 5"
    , makeTest
        "x = 5 ; y = x"
        "v0 = 5 ; v1 = v0"
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
    ]