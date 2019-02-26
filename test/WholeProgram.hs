{-# LANGUAGE QuasiQuotes #-}

module WholeProgram where

import Test.Tasty
import Test.Tasty.HUnit

import BasicPrelude
import Compiler          (Flags(outputJar))
import Data.Default      (def)
import Data.Text         (unpack)
import NeatInterpolation
import System.Exit       (ExitCode(ExitSuccess))
import System.IO.Temp
import System.Process    (readProcessWithExitCode)


makeTest :: (String, Text, String) -> TestTree
makeTest (title, source, expected) = testCase title $ do
    tempDir <- getCanonicalTemporaryDirectory
    withTempDirectory tempDir "compiler-test" $ \dir -> do
        sourceFile <- writeTempFile dir "compiler-test" (unpack source)
        let buildArgs = ["exec", "compiler-exe", "--", "-v", "-d", dir, dir </> sourceFile]
            runArgs = ["-noverify", "-jar", dir </> outputJar def]
        (buildResult, buildOutput, buildErr) <- readProcessWithExitCode "stack" buildArgs ""
        unless (buildResult == ExitSuccess) $ assertFailure $ intercalate "\n" [buildOutput, buildErr]
        (runResult, runOutput, runErr) <- readProcessWithExitCode "java" runArgs ""
        unless (runResult == ExitSuccess) $ assertFailure $ intercalate "\n" [runOutput, runErr]
        assertEqual buildOutput expected runOutput

test :: TestTree
test = testGroup "Whole Program" $ map makeTest
    [
        (
            "_main = True"
        ,
            [text|
                data Bool = False | True
                data (,) a = (,) a
                _main = True
            |]
        ,
            "Data: { branch: 1, data: { } }\n"
        )
    ,
        (
            "_main = id id False"
        ,
            [text|
                data Bool = False | True
                data (,) a = (,) a
                id = \x -> x
                _main = id id False
            |]
        ,
            "Data: { branch: 0, data: { } }\n"
        )
    ,
        (
            "_main = foo [True, False]"
        ,
            [text|
                data Bool = False | True
                data [] a = [] | a : [a]
                data (,) a = (,) a

                (&&) = \x y -> case x of
                    False -> False
                    True -> case y of
                        False -> False
                        True -> True

                all = \f xl -> case xl of
                    [] -> True
                    (x:xs) -> f x && all f xs

                class Foo a where
                    foo :: a -> Bool
                instance Foo Bool where
                    foo = \x -> x
                instance Foo [Bool] where
                    foo = all foo

                _main = foo [True, False]
            |]
        ,
            "Data: { branch: 0, data: { } }\n"
        )
    ,
        (
            "_main = (True, False, True)"
        ,
            [text|
                data (,,) a b c = (,,) a b c
                data Bool = False | True

                _main = (True, False, True)
            |]
        ,
            "Data: { branch: 0, data: { Data: { branch: 1, data: { } } Data: { branch: 0, data: { } } Data: { branch: 1, data: { } } } }\n"
        )
    ,
        (
            "_main = (\\(Foo x) -> x) (Foo True)"
        ,
            [text|
                data Bool = False | True
                data (,) a = (,) a

                data Foo a = Foo a

                _main = (\(Foo x) -> x) (Foo True)
            |]
        ,
            "Data: { branch: 1, data: { } }\n"
        )
    ,
        (
            "[x, y] = [False, True]"
        ,
            [text|
                data Bool = False | True
                data [] a = [] | a : [a]

                [x, y] = [False, True]

                _main = x
            |]
        ,
            "Data: { branch: 0, data: { } }\n"
        )
    ,
        (
            "_main = all not [False, True]"
        ,
            [text|
                data Bool = False | True
                data [] a = [] | a : [a]

                True && True = True
                _ && _ = False

                all f [] = True
                all f (x:xs) = f x && all f xs

                not True = False
                not False = True

                _main = all not [False, False]
            |]
        ,
            "Data: { branch: 1, data: { } }\n"
        )
    ,
        (
            "_main = sum [1,2,3,4,5,6,7,8,9,10]"
        ,
            [text|
                data Int
                data Bool = False | True
                data [] a = [] | a : [a]
                            
                class Num a where
                    (+) :: a -> a -> a
                instance Num Int where
                    (+) = primNumIntAdd
                class Eq a where
                    (==) :: a -> a -> Bool
                instance Eq Int where
                    (==) = primEqIntEq
                
                primNumIntAdd :: Int -> Int -> Int
                primEqIntEq :: Int -> Int -> Bool

                foldl _ a [] = a
                foldl f a (x:xs) = foldl f (f a x) xs

                sum = foldl (+) 0

                _main = sum [1,2,3,4,5,6,7,8,9,10] :: Int
            |]
        ,
            "Int: 55\n"
        )
    ,
        (
            "_main = factorial 10"
        ,
            [text|
                data Int
                data [] a = [] | a : [a]
                data Bool = False | True

                class Num a where
                    (-) :: a -> a -> a
                    (*) :: a -> a -> a
                instance Num Int where
                    (-) = primNumIntSub
                    (*) = primNumIntMult
                
                primNumIntAdd :: Int -> Int -> Int
                primNumIntSub :: Int -> Int -> Int
                primNumIntMult :: Int -> Int -> Int
                
                class Eq a where
                    (==) :: a -> a -> Bool
                instance Eq Int where
                    (==) = primEqIntEq
                
                primEqIntEq :: Int -> Int -> Bool
                
                factorial 0 = 1
                factorial n = n * factorial (n - 1)
                
                _main = factorial 10 :: Int
            |]
        ,
            "Int: 3628800\n"
        )
    ]
