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
                factorial 0 = 1
                factorial n = n * factorial (n - 1)
                
                _main = factorial 10 :: Int
            |]
        ,
            "Int: 3628800\n"
        )
    ,
        (
            "_main = factorial 50"
        ,
            [text|
                factorial 0 = 1
                factorial n = n * factorial (n - 1)
                
                _main = factorial 50 :: Integer
            |]
        ,
            "Integer: 30414093201713378043612608166064768844377641568960512000000000000\n"
        )
    ]
