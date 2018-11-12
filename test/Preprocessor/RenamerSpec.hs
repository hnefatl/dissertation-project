module Preprocessor.RenamerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Data.Text.Lazy (unpack)
import Text.Pretty.Simple

import ExtraDefs
import Preprocessor.Renamer

makeTest :: String -> String -> TestTree
makeTest input expected = testCase (deline input) $ case (,) <$> parseModule input <*> parseModule expected of
    ParseOk (input', HsModule _ _ _ _ expectedDecls) ->
        let (renamedInput, state) = runRenamer (renameModule input')
        in case renamedInput of
            Right (HsModule _ _ _ _ renamedDecls) -> assertEqual (unpack $ pShow state) renamedDecls expectedDecls
            Left err -> assertFailure err
    ParseFailed loc msg -> assertFailure ("Failed to parse input: " ++ show loc ++ "\n" ++ msg)

-- The unique names should be alphabetical "per-layer": each layer of names are lumped into a set and assigned in order
-- We cludge some variable names to be eg. "xx" because when the uniquified name "v0" is parsed it adjusts source
-- location tags by two characters, whereas "x" only adjusts them by 1.
test :: TestTree
test = testGroup "Renamer"
    [ makeTest "x = 5" "v0 = 5"
    , makeTest "x = 5\ny = x" "v0 = 5\nv1 = v0"
    , makeTest "x = y\ny = x" "v0 = v1\nv1 = v0"
    , makeTest "(a, b) = (x, 4)\nx = 1" "(v0, v1) = (v2, 4)\nv2 = 1"
    , makeTest "(_, a) = 4" "(_, v0) = 4"
    , makeTest "x@(a, b) = (1, 2)" "v2@(v0, v1) = (1, 2)"
    , makeTest "x = 2\n[a,2,c] = [1,x,3]" "v2 = 2\n[v0,2,v1] = [1,v2,3]"
    , makeTest "_ = let {xx=0;yy=1;zz=2} in if xx then yy else zz" "_ = let {v0=0;v1=1;v2=2} in if v0 then v1 else v2"
    , makeTest "_ = \\xx -> xx" "_ = \\v0 -> v0"
    , makeTest "_ = let {xx=0;yy=(\\xx -> xx)} in yy" "_ = let {v0=0;v1=(\\v2 -> v2)} in v1"
    ]