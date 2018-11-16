module Preprocessor.DependencySpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser
import Language.Haskell.Syntax
import Control.Monad.Except
import Text.Printf

import ExtraDefs
import Preprocessor.Dependency

makeTest :: String -> [[String]] -> TestTree
makeTest input cases = testCase (deline input) $ case parseModule input of
    ParseFailed loc msg -> assertFailure ("Failed to parse input: " ++ show loc ++ "\n" ++ msg)
    ParseOk (HsModule _ _ _ _ decls) -> case runExcept (dependencyOrder decls) of
        Left err -> assertFailure err
        Right declOrder
            | length declOrder /= length cases -> assertFailure $ printf "Lengths: %s vs %s" (show cases) (show decls)
            | otherwise -> forM_ (zip cases declOrder) $ \(expectedGroup, actualGroup) -> do
                undefined



test :: TestTree
test = testGroup "Dependency Analysis" $
    [

    ]