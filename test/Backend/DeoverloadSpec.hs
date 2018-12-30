module Backend.DeoverloadSpec where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (assertFailure, testCase)

import Language.Haskell.Parser

import BasicPrelude
import Control.Monad.Except    (runExcept, runExceptT)
import Data.Text               (unpack)
import Formatting              (sformat, stext, (%))

import AlphaEq
import Backend.Deoverload
import ExtraDefs               (deline, pretty, synPrint)
import Logger                  (runLogger, runLoggerT)
import NameGenerator
import Typechecker.Hardcoded
import Typechecker.Typechecker

unpackEither :: Either e a -> (e -> Text) -> IO a
unpackEither (Left err) f = assertFailure $ unpack (f err)
unpackEither (Right x) _  = return x

makeTest :: Text -> Text -> TestTree
makeTest sActual sExpected =
    testCase (unpack $ deline sActual) $ case (parseModule $ unpack sExpected, parseModule $ unpack sActual) of
        (ParseOk expected, ParseOk actualModule) -> do
            let infer = runTypeInferrer (inferModuleWithBuiltins actualModule)
                (((eTiOutput, tState), inferLogs), i) = runNameGenerator (runLoggerT infer) 0
            (taggedModule, ts) <- unpackEither (runExcept eTiOutput) id
            let deoverload = runDeoverload $ do
                    addTypes ts
                    addClassEnvironment builtinClasses
                    addDictionaries builtinDictionaries
                    deoverloadModule taggedModule
                ((eDeoverloaded, dState), deoverloadLogs) = evalNameGenerator (runLoggerT deoverload) i
                prettified m = unlines [synPrint expected', synPrint m]
                deoverloadMsg = unlines ["Expected:", synPrint expected', "Tagged:", synPrint taggedModule, pretty taggedModule]
                assertMsg actual = unlines ["Expected:", pretty expected', "Got:", pretty actual]
                expected' = stripModuleParens expected
            actual <- unpackEither (runExcept eDeoverloaded) (\err -> unlines [err, prettified taggedModule, deoverloadMsg, pretty tState, pretty dState])
            let (result, alphaLogs) = runLogger $ runExceptT $ alphaEqError expected' actual
            unpackEither result (\err -> unlines [err, prettified actual, assertMsg actual, pretty dState])
        (ParseFailed _ _, _) -> assertFailure "Failed to parse expected"
        (_, ParseFailed _ _) -> assertFailure "Failed to parse actual"

test :: TestTree
test = testGroup "Deoverload"
    [ let
        x = "((x :: Num a -> a) (d :: Num a) :: a)"
        addType = "((+) :: Num a -> a -> a -> a)"
      in makeTest
        "f = \\x -> x + x" $
        "f = (\\d -> (\\x -> ((" <> addType <> " (d :: Num a) :: a -> a -> a) " <> x <> " :: a -> a) " <> x <> " :: a) :: a -> a) :: Num a -> a -> a"
    , makeTest
        "x = if True then 0 else 1"
        "x = (\\d -> (if True :: Bool then 0 :: a else 1 :: a) :: a) :: Num a -> a"
    , makeTest
        "x = [1]"
        "x = (\\d -> [1 :: a] :: [a]) :: Num a -> [a]"
    , makeTest
        "x = (\\y -> [0, y])"
        "x = (\\d -> (\\y -> [0 :: a, (y :: Num a -> a) (d :: Num a) :: a] :: [a]) :: a -> [a]) :: Num a -> a -> [a]"
    , makeTest
        "(x, y) = (True, [1])"
        "(x, y) = (\\d -> (True :: Bool, [1 :: a] :: [a]) :: (Bool, [a])) :: Num a -> (Bool, [a])"
    , makeTest
        "f = \\x -> x ; y = f 0"
        "f = (\\x -> x :: a) :: a -> a ; y = (\\d -> (f :: b -> b) (0 :: b) :: b) :: Num b -> b"
    , makeTest
        "f = \\x -> x ; y = f True"
        "f = (\\x -> x :: a) :: a -> a ; y = (f :: Bool -> Bool) (True :: Bool) :: Bool"
    , makeTest
        "f = \\x -> x + x ; y = f (0 :: Int)"
        "f = (\\d -> (\\x -> ((((+) :: Num a -> a -> a -> a) (d :: Num a) :: a -> a -> a) ((x :: Num a -> a) (d :: Num a) :: a) :: a -> a) ((x :: Num a -> a) (d :: Num a) :: a) :: a) :: a -> a) :: Num a -> a -> a ; y = ((f :: Num Int -> Int -> Int) (dNumInt :: Num Int) :: Int -> Int) (((0 :: Int) :: Int) :: Int) :: Int"
    , let
        a = unlines
            [ "const = \\x _ -> x"
            , "f = \\y z -> const (y == y) (z + z)"
            , "g = f True (1 :: Int)" ]
        -- Subexpressions of the expected expression
        y = "(y :: Eq c -> c) (dc :: Eq c) :: c"
        z = "(z :: Num d -> d) (dd :: Num d) :: d"
        eq = "((==) :: Eq c -> c -> c -> Bool) (dc :: Eq c) :: c -> c -> Bool"
        plus = "((+) :: Num d -> d -> d -> d) (dd :: Num d) :: d -> d -> d"
        yeqy = sformat ("(("%stext%") ("%stext%") :: c -> Bool) ("%stext%") :: Bool") eq y y
        zplusz = sformat ("(("%stext%") ("%stext%") :: d -> d) ("%stext%") :: d") plus z z
        constapp = sformat ("((const :: Bool -> d -> Bool) ("%stext%") :: d -> Bool) ("%stext%") :: Bool") yeqy zplusz
        bbody = sformat ("(\\y z -> ("%stext%")) :: c -> d -> Bool") constapp
        bdicts = sformat ("(\\dc dd -> ("%stext%")) :: Eq c -> Num d -> c -> d -> Bool") bbody
        b = unlines
            [ "const = (\\x _ -> x :: a) :: a -> b -> a"
            , "f = " <> bdicts
            , "g = ((((f :: Eq Bool -> Num Int -> Bool -> Int -> Bool) (dEqBool :: Eq Bool) :: Num Int -> Bool -> Int -> Bool) (dNumInt :: Num Int) :: Bool -> Int -> Bool) (True :: Bool) :: Int -> Bool) (((1 :: Int) :: Int) :: Int) :: Bool"
            ]
      in makeTest a b
    ]
