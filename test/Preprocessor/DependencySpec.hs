{-# Language OverloadedStrings #-}

module Preprocessor.DependencySpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Control.Monad.Except
import Text.Printf
import Text.Pretty.Simple
import qualified Data.Set as S

import ExtraDefs
import Preprocessor.Dependency
import Preprocessor.ContainedNames

makeTest :: String -> [[String]] -> TestTree
makeTest input cases = testCase (deline input) $ case parseModule input of
    ParseFailed loc msg -> assertFailure ("Failed to parse input: " ++ show loc ++ "\n" ++ msg)
    ParseOk (HsModule _ _ _ _ decls) -> case runExcept (dependencyOrder decls) of
        Left err -> assertFailure err
        Right declOrder
            | length declOrder /= length cases -> assertFailure $ printf "Lengths: %s vs %s" (pShow cases) (pShow decls)
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) ->
                case runExcept $ getDeclsBoundNames actualGroup of
                    Left err -> assertFailure err
                    Right boundNames -> assertEqual "" boundNames (S.fromList $ map VariableName expectedGroup)



test :: TestTree
test = testGroup "Dependency Analysis"
    [ makeTest "f = \\x -> x\ng = \\y -> y" [["g"], ["f"]]
    , makeTest "f = \\x -> x\ng = \\y -> y\nh = \\z -> z" [["h"], ["g"], ["f"]]
    , makeTest "f = \\x -> x\ng = \\y -> f y" [["f"], ["g"]]
    , makeTest "f = \\x -> g x\ng = \\y -> f y" [["f", "g"]]
    , makeTest "f = \\x -> x\ng = \\y -> h y\nh = \\z -> g z" [["g", "h"], ["f"]]
    , makeTest "f = \\x -> x\ng = \\y -> f (h y)\nh = \\z -> g z" [["f"], ["g", "h"]]
    , makeTest "f = \\a -> a\ng = \\b -> f (h b)\nh = \\c -> i c\ni = \\d -> g (j d)\nj = \\e -> i e" [["f"], ["g", "h", "i", "j"]]
    , makeTest "(a,b) = (1,2)\nc = a" [["a", "b"], ["c"]]
    , makeTest "x@(a,_,b) = (1,2,3)\nc = a\nd = b\ne = (c, d)" [["x", "a", "b"], ["c"], ["d"], ["e"]]
    , makeTest "x = if a then b else c\na = b\nb = c\nc = True" [["c"], ["b"], ["a"], ["x"]]
    , makeTest "x = if a then b else c\na = b\nb = a\nc = True" [["c"], ["a", "b"], ["x"]]
    ]