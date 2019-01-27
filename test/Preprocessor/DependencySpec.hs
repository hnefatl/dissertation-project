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
import           Preprocessor.Dependency     (dependencyOrder, getDepBoundVariables)
import           TextShowHsSrc               ()

tvToV :: Either VariableName TypeVariableName -> VariableName
tvToV (Left v) = v
tvToV (Right (TypeVariableName n)) = VariableName n

makeTest :: Text -> [[VariableName]] -> TestTree
makeTest input cases = testCase (unpack $ deline input) $ case parseModule (unpack input) of
    ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg
    ParseOk (HsModule _ _ _ _ decls) -> case runLogger $ evalNameGeneratorT (runExceptT $ dependencyOrder decls) 0 of
        (Left err, logs) -> assertFailure $ unpack $ unlines [err, unlines logs]
        (Right declOrder, logs)
            | length declOrder /= length cases ->
                assertFailure $ unpack $ unlines ["Lengths:", pretty cases, "vs", pretty declOrder, unlines logs]
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) ->
                --case runExcept (S.union (S.map tvToV $ getBoundTypeConstants actualGroup) <$> getBoundVariables actualGroup) of
                case runExcept $ evalNameGeneratorT (S.map tvToV . S.unions <$> mapM getDepBoundVariables actualGroup) 0 of
                    Left err         -> assertFailure $ unpack err
                    Right boundNames -> assertEqual (unpack $ unlines logs) (S.fromList expectedGroup) boundNames



test :: TestTree
test = testGroup "Dependency Analysis"
    [makeTest "f = \\x -> x ; g = \\y -> y" [["g"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> y ; h = \\z -> z" [["h"], ["g"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> f y" [["f"], ["g"]]
    , makeTest "f = \\x -> g x ; g = \\y -> f y" [["f", "g"]]
    , makeTest "f = \\x -> x ; g = \\y -> h y ; h = \\z -> g z" [["g", "h"], ["f"]]
    , makeTest "f = \\x -> x ; g = \\y -> f (h y) ; h = \\z -> g z" [["f"], ["g", "h"]]
    , makeTest
        "f = \\a -> a ; g = \\b -> f (h b) ; h = \\c -> i c ; i = \\d -> g (j d) ; j = \\e -> i e"
        [["f"], ["g", "h", "i", "j"]]
    , makeTest "(a,b) = (1,2) ; c = a" [["a", "b"], ["c"]]
    , makeTest "x@(a,_,b) = (1,2,3) ; c = a ; d = b ; e = (c, d)" [["x", "a", "b"], ["c"], ["d"], ["e"]]
    , makeTest "x = if a then b else c ; a = b ; b = c ; c = True" [["c"], ["b"], ["a"], ["x"]]
    , makeTest "x = if a then b else c ; a = b ; b = a ; c = True" [["c"], ["a", "b"], ["x"]]
    , makeTest "_ = 1" [[]]
    , makeTest "data Bool = True | False ; x = True" [["Bool", "True", "False"], ["x"]]
    , makeTest "x = True ; data Bool = True | False " [["Bool", "True", "False"], ["x"]]
    , makeTest "data Bool = True | False ; x True = 0" [["Bool", "True", "False"], ["x"]]
    , makeTest "x 1 () = 0 ; data () = ()" [["()"], ["x"]]
    , makeTest "data [] a = [] | a :+ [a] ; f = \\x -> case x of { [] -> False ; _:+_ -> True }" [["[]", ":+"], ["f"]]
    , makeTest
        "data [] a = [] | a :+ [a] ; data Bool = False | True ; x = True:+False:+[]"
        [["Bool", "False", "True"], ["[]", ":+"], ["x"]]
    , makeTest "data Bool = False | True ; x :: Bool" [["Bool", "False", "True"], ["x"]]
    , makeTest "data () = () ; x :: () -> ()" [["()"], ["x"]]
    , makeTest
        "data () = () ; class F a where { f :: () -> a } ; instance F () where { f () = () }"
        [["()"], ["F", "f"], []]
    , makeTest
        "data Bool = False | True ; data [] a = [] | a :+ [a] ; class Foo a where { f :: a -> Bool } ; instance Foo Bool where { f = \\x -> x } ; instance Foo [Bool] where { f = all f } ; any :: (a -> Bool) -> [a] -> Bool"
        [["Bool", "True", "False"], ["Foo", "f"], [], ["[]", ":+"], ["any"], []]
    ]
