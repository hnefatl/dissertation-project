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
                data [] a = [] | a :+ [a]
                data (,) a = (,) a

                (&&) = \x y -> case x of
                    False -> False
                    True -> case y of
                        False -> False
                        True -> True

                all = \f xl -> case xl of
                    [] -> True
                    (x:+xs) -> f x && all f xs

                class Foo a where
                    foo :: a -> Bool
                instance Foo Bool where
                    foo = \x -> x
                instance Foo [Bool] where
                    foo = all foo

                _main = foo (True:+(False:+[]))
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
            "Data: { branch: 0, data: { } }\n"
        )
    ]