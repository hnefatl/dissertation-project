{-# LANGUAGE OverloadedStrings #-}

module Preprocessor.DependencySpec where

import           Test.Tasty                  (TestTree, testGroup)
import           Test.Tasty.HUnit            (assertEqual, assertFailure, testCase)

import           BasicPrelude
import           Control.Monad.Except        (runExcept, runExceptT)
import qualified Data.Set                    as S
import           Data.Text                   (pack, unpack)
import           Language.Haskell.Parser     (ParseResult(..), parseModule)
import           Language.Haskell.Syntax
import           TextShow                    (showt)

import           ExtraDefs
import           Logger                      (runLogger)
import           NameGenerator               (evalNameGeneratorT)
import           Names
import           Preprocessor.ContainedNames
import           Preprocessor.Dependency     (dependencyOrder)
import           TextShowHsSrc               ()

makeTest :: Text -> [[VariableName]] -> TestTree
makeTest input cases = testCase (unpack $ deline input) $ case parseModule (unpack input) of
    ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg
    ParseOk (HsModule _ _ _ _ decls) -> case runLogger $ evalNameGeneratorT (runExceptT $ dependencyOrder decls) 0 of
        (Left err, logs) -> assertFailure $ unpack $ unlines [err, unlines logs]
        (Right declOrder, logs)
            | length declOrder /= length cases ->
                assertFailure $ unpack $ "Lengths: " <> pretty cases <> " vs " <> pretty declOrder
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) ->
                case runExcept (getBoundVariables actualGroup) of
                    Left err         -> assertFailure $ unpack err
                    Right boundNames -> assertEqual (unpack $ unlines logs) boundNames (S.fromList expectedGroup)



test :: TestTree
test = testGroup "Dependency Analysis"
    [makeTest "f = \\x -> x ; g = \\y -> y" [["g"], ["f"]]
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
    , makeTest "data Bool = True | False ; x = True" [["True", "False"], ["x"]]
    , makeTest "x = True ; data Bool = True | False " [["True", "False"], ["x"]]
    ]
