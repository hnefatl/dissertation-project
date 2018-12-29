module Typechecker.TypeTaggerSpec where

import BasicPrelude
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (assertBool, assertFailure, testCase)

import Control.Monad.Except
import Data.Text               (unpack)
import Language.Haskell.Parser (ParseResult(..), parseModule)

import AlphaEq
import ExtraDefs               (deline, pretty, synPrint)
import Logger
import NameGenerator
import Typechecker.Typechecker

makeTest :: Text -> Text -> TestTree
makeTest input expected = testCase (unpack $ deline input) $ do
    (inputModule, expectedModule) <- case (parseModule $ unpack input, parseModule $ unpack expected) of
        (ParseFailed _ msg, _) -> assertFailure $ "Failed to parse input: " <> msg
        (_, ParseFailed _ msg) -> assertFailure $ "Failed to parse expected: " <> msg
        (ParseOk m, ParseOk n) -> return (m, n)
    let expectedModule' = stripModuleParens expectedModule
        ((eModule, inferrerState), logs) = evalNameGenerator (runLoggerT $ runTypeInferrer $ inferModuleWithBuiltins inputModule) 0
    case runExcept eModule of
        Left msg -> assertFailure $ unpack $ unlines ["Failed to generate tagged tree: ", msg, "Logs:", unlines logs]
        Right (inputModule', _) -> assertBool err $ alphaEq inputModule' expectedModule'
            where err = unpack $ unlines
                    [ "Expected:", synPrint expectedModule', "Got:", synPrint inputModule', "Expected Tree:"
                    , pretty expectedModule', "Got Tree:", pretty inputModule', "State:", pretty inferrerState
                    , "Logs:", unlines logs ]

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
