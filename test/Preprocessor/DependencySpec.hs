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
import Names
import NameGenerator
import Preprocessor.Dependency
import Preprocessor.ContainedNames

makeTest :: String -> [[String]] -> TestTree
makeTest input cases = testCase (deline input) $ case parseModule input of
    ParseFailed loc msg -> assertFailure ("Failed to parse input: " ++ show loc ++ "\n" ++ msg)
    ParseOk (HsModule _ _ _ _ decls) -> case runExcept $ evalNameGeneratorT (dependencyOrder decls) 0 of
        Left err -> assertFailure err
        Right declOrder
            | length declOrder /= length cases ->
                assertFailure $ printf "Lengths: %s vs %s" (pShow cases) (pShow declOrder)
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) ->
                case runExcept (getDeclsBoundNames actualGroup) of
                    Left err -> assertFailure err
                    Right boundNames -> assertEqual "" boundNames (S.fromList $ map VariableName expectedGroup)



test :: TestTree
test = testGroup "Dependency Analysis"
    [ makeTest "f = \\x -> x ; g = \\y -> y" [["g"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> y ; h = \\z -> z" [["h"], ["g"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> f y" [["f"], ["g"]]
    , makeTest "f = \\x -> g x ; g = \\y -> f y" [["f", "g"]]
    , makeTest "f = \\x -> x ; g = \\y -> h y ; h = \\z -> g z" [["g", "h"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> f (h y) ; h = \\z -> g z" [["f"], ["g", "h"]]
    , makeTest "f = \\a -> a ; g = \\b -> f (h b) ; h = \\c -> i c ; i = \\d -> g (j d) ; j = \\e -> i e" [["f"], ["g", "h", "i", "j"]]
    , makeTest "(a,b) = (1,2) ; c = a" [["a", "b"], ["c"]]
    , makeTest "x@(a,_,b) = (1,2,3) ; c = a ; d = b ; e = (c, d)" [["x", "a", "b"], ["c"], ["d"], ["e"]]
    , makeTest "x = if a then b else c ; a = b ; b = c ; c = True" [["c"], ["b"], ["a"], ["x"]]
    , makeTest "x = if a then b else c ; a = b ; b = a ; c = True" [["c"], ["a", "b"], ["x"]]
    , makeTest "_ = 1" [[]]
    ]