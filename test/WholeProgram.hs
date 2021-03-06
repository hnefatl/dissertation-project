{-# LANGUAGE QuasiQuotes #-}

module WholeProgram where

import           Test.Tasty
import           Test.Tasty.HUnit

import           BasicPrelude      hiding (unwords)
import           Data.List         (unwords)
import qualified Data.Set          as S
import           Data.Text         (pack, strip, unpack)
import           NeatInterpolation
import           System.Exit       (ExitCode(ExitSuccess))
import           System.IO.Temp    (withSystemTempDirectory)
import           System.Process    (readProcessWithExitCode)


makeTest :: (String, Text, String) -> [TestTree]
makeTest (title, source, expected) = map makeTest' (S.toList $ S.powerSet optimisations)
    where optimisations = S.fromList ["-l", "-t", "-u"]
          makeTest' opts = do
            let optList = S.toList opts
            testCase (makeTitle title optList) $
                withSystemTempDirectory "compiler-test" $ \dir -> do
                    let sourceFile = dir </> "testsrc.hs"
                        outputJar = dir </> "a.jar"
                    writeFile sourceFile source
                    let buildArgs = ["exec", "compiler-exe", "--", "-v", "-d", dir, "-o", outputJar, sourceFile] <> optList
                        runArgs = ["-noverify", "-jar", outputJar]
                    (buildResult, buildOutput, buildErr) <- readProcessWithExitCode "stack" buildArgs ""
                    unless (buildResult == ExitSuccess) $ assertFailure $ intercalate "\n" [buildOutput, buildErr]
                    (runResult, runOutput, runErr) <- readProcessWithExitCode "java" runArgs ""
                    unless (runResult == ExitSuccess) $ assertFailure $ intercalate "\n" [runOutput, runErr]
                    assertEqual buildOutput expected (unpack $ strip $ pack runOutput)


makeTitle :: String -> [String] -> String
makeTitle title []   = title
makeTitle title opts = unwords $ title:"with":opts

test :: TestTree
test = testGroup "Whole Program" $ concatMap makeTest
    [
        (
            "main = show True"
        ,
            [text|
                main = show True
            |]
        ,
            "True"
        )
    ,
        (
            "main = show (id id False)"
        ,
            [text|
                main = show (id id False)
            |]
        ,
            "False"
        )
    ,
        (
            "main = show (foo [True, False])"
        ,
            [text|
                class Foo a where
                    foo :: a -> Bool
                instance Foo Bool where
                    foo = \x -> x
                instance Foo [Bool] where
                    foo = all foo

                main = show (foo [True, False])
            |]
        ,
            "False"
        )
    ,
        (
            "main = show (snd (True, 0 :: Int))"
        ,
            [text|
                snd (_, y) = y

                main = show (snd (True, 0 :: Int))
            |]
        ,
            "0"
        )
    ,
        (
            "main = show ((\\(Foo x) -> x) (Foo True))"
        ,
            [text|
                data Foo a = Foo a

                main = show ((\(Foo x) -> x) (Foo True))
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
    ,
        (
            "diss-code-1"
        ,
            [text|
                ones = 1:ones
                summed = sum (take 10 ones :: [Int]) == 10

                main = show summed
            |]
        ,
            "True"
        )
    ,
        (
            "diss-code-2"
        ,
            [text|
                divide x y = if y == 0 then Nothing else Just (x + y)
                x = divide (4 :: Int) 0 >>= divide 20

                countdown 0 = []
                countdown n = n:countdown (n - 1)
                onlyEven x = if even x then [x] else []
                y = [1 :: Int,2,3] >>= countdown
                z = y >>= onlyEven

                main = show x ++ (show y ++ (show z))
            |]
        ,
            "Nothing[1,2,1,3,2,1][2,2]"
        )
    ,
        (
            "mergesort"
        ,
            [text|
                mergesort [] = [] :: [Int]
                mergesort [x] = [x]
                mergesort [x, y] = if x < y then [x, y] else [y, x]
                mergesort zs = let (xs, ys) = split zs in merge (mergesort xs) (mergesort ys)

                split [] = ([], []) :: ([Int], [Int])
                split [x] = ([x], [])
                split (x:(y:zs)) = let (xs, ys) = split zs in (x:xs, y:ys)

                merge [] ys = ys
                merge xs [] = xs
                merge (x:xs) (y:ys) = if x < y then x:merge xs (y:ys) else y:merge (x:xs) ys

                list = [7,3,5,6,2,10,4,1,8,9] :: [Int]

                main = show (mergesort list)
            |]
        ,
            "[1,2,3,4,5,6,7,8,9,10]"
        )
    ]
