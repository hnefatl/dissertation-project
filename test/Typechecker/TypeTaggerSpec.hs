module Typechecker.TypeTaggerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Language.Haskell.Pretty
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
    let expectedModule' = stripModuleParens expectedModule
        (eModule, inferrerState) = evalNameGenerator (runTypeInferrer $ inferModuleWithBuiltins inputModule) 0
    case runExcept eModule of
        Left msg -> assertFailure $ "Failed to generate tagged tree: " ++ msg
        Right (inputModule', _) -> assertBool err $ alphaEq inputModule' expectedModule'
            where
                format x = unpack $ pShow x
                msg = "Expected:\n%s\nGot:\n%s\nExpected Tree:\n%s\nGot Tree:\n%s\nState:%s"
                err = printf msg (prettyPrint expectedModule') (prettyPrint inputModule') (format expectedModule') (format inputModule') (format inferrerState)

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