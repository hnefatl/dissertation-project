{-# Language QuasiQuotes #-}

module WholeProgram where

import           Test.Tasty
import           Test.Tasty.HUnit

import           BasicPrelude
import           NeatInterpolation
import           Data.Text               (unpack)
import           Data.Default            (def)
import           System.Process          (readProcessWithExitCode)
import           System.Exit             (ExitCode(ExitSuccess))
import           System.IO.Temp
import           Compiler (Flags(outputJar))


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
    ]