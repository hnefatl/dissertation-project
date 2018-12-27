module Typechecker.TypeTaggerSpec where

import BasicPrelude
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertBool)

import TextShow (TextShow, showt)
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Pretty (Pretty, prettyPrint)
import Control.Monad.Except
import Data.Text (unpack, pack)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pString)

import AlphaEq
import ExtraDefs
import NameGenerator
import Typechecker.Typechecker

pretty :: TextShow a => a -> Text
pretty = toStrict . pString . unpack . showt
synPrint :: Pretty a => a -> Text
synPrint = pack . prettyPrint

makeTest :: Text -> Text -> TestTree
makeTest input expected = testCase (unpack $ deline input) $ do
    (inputModule, expectedModule) <- case (parseModule $ unpack input, parseModule $ unpack expected) of
        (ParseFailed _ msg, _) -> assertFailure $ "Failed to parse input: " <> msg
        (_, ParseFailed _ msg) -> assertFailure $ "Failed to parse expected: " <> msg
        (ParseOk m, ParseOk n) -> return (m, n)
    let expectedModule' = stripModuleParens expectedModule
        (eModule, inferrerState) = evalNameGenerator (runTypeInferrer $ inferModuleWithBuiltins inputModule) 0
    case runExcept eModule of
        Left msg -> assertFailure $ unpack $ "Failed to generate tagged tree: " <> msg
        Right (inputModule', _) -> assertBool err $ alphaEq inputModule' expectedModule'
            where err = unpack $ unlines
                    [ "Expected:", synPrint expectedModule', "Got:", synPrint inputModule', "Expected Tree:"
                    , pretty expectedModule', "Got Tree:", pretty inputModule', "State:", pretty inferrerState ]

test :: TestTree
test = testGroup "Type Tagger"
    [ makeTest
        "x = 5"
        "x = 5 :: Num a => a"
    , makeTest
        "x = (+) 1"
        "x = ((+) :: Num a => a -> a -> a) (1 :: Num a => a) :: Num a => a -> a"
    , makeTest
        "x = 1 + 2"
        "x = (((+) :: Num a => a -> a -> a) (1 :: Num a => a) :: Num a => a -> a) (2 :: Num a => a) :: Num a => a"
    , makeTest
        "id = \\x -> x ; f = id True"
        "id = (\\x -> x :: a) :: a -> a ; f = (id :: Bool -> Bool) (True :: Bool) :: Bool"
    , makeTest
        "x = (\\[y, z] -> y + z) [1, 2]"
        "x = ((\\[y, z] -> (((+) :: Num a => a -> a -> a) (y :: Num a => a) :: Num a => a -> a) (z :: Num a => a) :: Num a => a) :: Num a => [a] -> a) ([1 :: Num a => a, 2 :: Num a => a] :: Num a => [a]) :: Num a => a"
    , makeTest
        "x = True ; y = (if x then x else False, 1)"
        "x = True :: Bool ; y = ((if x :: Bool then x :: Bool else False :: Bool) :: Bool, 1 :: Num a => a) :: Num a => (Bool, a)"
    ]