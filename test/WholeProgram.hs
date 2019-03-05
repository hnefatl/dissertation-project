{-# LANGUAGE QuasiQuotes #-}

module WholeProgram where

import Test.Tasty
import Test.Tasty.HUnit

import BasicPrelude
import Compiler          (Flags(outputJar))
import Data.Default      (def)
import Data.Text         (unpack, pack, strip)
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
        assertEqual buildOutput expected (unpack $ strip $ pack runOutput)

test :: TestTree
test = testGroup "Whole Program" $ map makeTest
    [
        (
            "main = True"
        ,
            [text|
                main = True
            |]
        ,
            "True"
        )
    ,
        (
            "main = id id False"
        ,
            [text|
                main = id id False
            |]
        ,
            "False"
        )
    ,
        (
            "main = foo [True, False]"
        ,
            [text|
                class Foo a where
                    foo :: a -> Bool
                instance Foo Bool where
                    foo = \x -> x
                instance Foo [Bool] where
                    foo = all foo

                main = foo [True, False]
            |]
        ,
            "False"
        )
    ,
        (
            "main = (True, False, True)"
        ,
            [text|
                main = (True, False, True)
            |]
        ,
            "Data: { branch: 0, data: { Data: { branch: 1, data: { } } Data: { branch: 0, data: { } } Data: { branch: 1, data: { } } } }"
        )
    ,
        (
            "main = (\\(Foo x) -> x) (Foo True)"
        ,
            [text|
                data Foo a = Foo a

                main = show (\(Foo x) -> x) (Foo True)
            |]
        ,
            "True"
        )
    ,
        (
            "[x, y] = [False, True]"
        ,
            [text|
                [x, y] = [False, True]
                main = show x
            |]
        ,
            "False"
        )
    ,
        (
            "main = all not [False, True]"
        ,
            [text|
                main = show (all not [False, False])
            |]
        ,
            "True"
        )
    ,
        (
            "main = sum [1,2,3,4,5,6,7,8,9,10]"
        ,
            [text|
                main = show (sum [1,2,3,4,5,6,7,8,9,10] :: Int)
            |]
        ,
            "55"
        )
    ,
        (
            "main = factorial 10"
        ,
            [text|
                factorial 0 = 1
                factorial n = n * factorial (n - 1)
                
                main = show (factorial 10 :: Int)
            |]
        ,
            "3628800"
        )
    ,
        (
            "main = factorial 50"
        ,
            [text|
                factorial 0 = 1
                factorial n = n * factorial (n - 1)
                
                main = show (factorial 50 :: Integer)
            |]
        ,
            "30414093201713378043612608166064768844377641568960512000000000000"
        )
    ]
