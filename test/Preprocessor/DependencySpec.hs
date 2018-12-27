{-# Language OverloadedStrings #-}

module Preprocessor.DependencySpec where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, assertEqual)

import BasicPrelude
import TextShow (TextShow, showt)
import Language.Haskell.Parser (parseModule, ParseResult(..))
import Language.Haskell.Syntax
import Control.Monad.Except (runExcept)
import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pShow)
import qualified Data.Set as S

import ExtraDefs
import Names
import TextShowHsSrc ()
import NameGenerator (evalNameGeneratorT)
import Preprocessor.Dependency (dependencyOrder)
import Preprocessor.ContainedNames

pretty :: TextShow a => a -> Text
pretty = toStrict . pShow . unpack . showt

makeTest :: Text -> [[Text]] -> TestTree
makeTest input cases = testCase (unpack $ deline input) $ case parseModule (unpack input) of
    ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg
    ParseOk (HsModule _ _ _ _ decls) -> case runExcept $ evalNameGeneratorT (dependencyOrder decls) 0 of
        Left err -> assertFailure $ unpack err
        Right declOrder
            | length declOrder /= length cases ->
                assertFailure $ unpack $ "Lengths: " <> pretty cases <> " vs " <> pretty declOrder
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) ->
                case runExcept (getDeclsBoundNames actualGroup) of
                    Left err -> assertFailure $ unpack err
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