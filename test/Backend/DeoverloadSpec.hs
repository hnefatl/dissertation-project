{-# Language TupleSections #-}

module Backend.DeoverloadSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Language.Haskell.Pretty

import Control.Monad.Except
import Data.Text.Lazy (unpack)
import Text.Printf
import Text.Pretty.Simple

import AlphaEq
import ExtraDefs
import NameGenerator
import Typechecker.Typechecker
import Typechecker.Hardcoded
import Backend.Deoverload

pretty :: Show a => a -> String
pretty = unpack . pShow

unpackEither :: Either e a -> (e -> String) -> IO a
unpackEither (Left err) f = assertFailure (f err)
unpackEither (Right x) _ = return x

format :: Show a => a -> String
format = unpack . pShow

makeTest :: String -> String -> TestTree
makeTest sActual sExpected = testCase (deline sActual) $ case (parseModule sExpected, parseModule sActual) of
    (ParseOk expected, ParseOk actualModule) -> do
        let action = do
                (m, res) <- inferModuleWithBuiltins actualModule
                (, res) <$> updateModuleTypeTags m
            infer = runTypeInferrer action
            ((eTiOutput, tState), i) = runNameGenerator infer 0
        (taggedModule, ts) <- unpackEither (runExcept eTiOutput) id
        let deoverload = runDeoverload $ do
                addTypes ts
                addClassEnvironment builtinClasses
                deoverloadModule taggedModule
            (eDeoverloaded, dState) = evalNameGenerator deoverload i
            prettified m = printf "%s\n%s" (prettyPrint expected') (prettyPrint m)
            deoverloadMsg = printf "Expected:\n%s\nTagged:\n%s\n%s" (prettyPrint expected') (prettyPrint taggedModule) (format taggedModule)
            assertMsg actual = printf "Expected:\n%s\nGot:\n%s" (format expected') (format actual)
            expected' = stripModuleParens expected
        actual <- unpackEither (runExcept eDeoverloaded) (\err -> unlines [err, prettified taggedModule, deoverloadMsg, pretty tState, pretty dState])
        assertBool (unlines [prettified actual, assertMsg actual, pretty dState]) (alphaEq expected' actual)
    (ParseFailed _ _, _) -> assertFailure "Failed to parse expected"
    (_, ParseFailed _ _) -> assertFailure "Failed to parse actual"

test :: TestTree
test = testGroup "Deoverload"
    [
    let x = "((x :: Num a -> a) (d :: Num a) :: a)"
        addType = "((+) :: Num a -> a -> a -> a)"
    in makeTest "f = \\x -> x + x" $
        printf "f = (\\d -> (\\x -> ((%s (d :: Num a) :: a -> a -> a) %s :: a -> a) %s :: a) :: a -> a) :: Num a -> a -> a" addType x x
    ,
        makeTest
            "x = if True then 0 else 1"
            "x = (\\d -> (if True :: Bool then 0 :: a else 1 :: a) :: a) :: Num a -> a"
    ,
        makeTest
            "(x, y) = (True, [1])"
            "(x, y) = (\\d -> (True :: Bool, [1 :: a] :: [a]) :: (Bool, [a])) :: Num a -> (Bool, [a])"
    ,
        makeTest
            "f = \\x -> x ; y = f 0"
            "f = (\\x -> x :: a) :: a -> a ; y = (\\d -> (f :: b -> b) (0 :: b) :: b) :: Num b -> b"
    ]