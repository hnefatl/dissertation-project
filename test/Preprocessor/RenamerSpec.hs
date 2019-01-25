module Preprocessor.RenamerSpec where

import           BasicPrelude
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (Assertion, assertFailure, testCase)

import           Language.Haskell.Parser (ParseResult(..), parseModule)

import           Control.Monad.Except    (runExcept)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           TextShow                (showt)

import           AlphaEq                 (AlphaEq, alphaEqError, stripModuleParens)
import           ExtraDefs               (deline, pretty, synPrint)
import           Logger                  (runLoggerT)
import           NameGenerator           (evalNameGenerator)
import           Names                   (VariableName)
import           Preprocessor.Renamer    (bindVariableForScope, renameModule, runRenamer)
import           Typechecker.Hardcoded   (builtinConstructors, builtinFunctions)

assertAlphaEq :: AlphaEq a => Text -> a -> a -> Assertion
assertAlphaEq msg x y = case runExcept $ alphaEqError x y of
    Left err -> assertFailure $ unpack $ unlines [err, msg]
    Right () -> return ()

makeTest :: Text -> Text -> TestTree
makeTest = makeTestWith (M.keysSet $ M.union builtinFunctions builtinConstructors)
makeVanillaTest :: Text -> Text -> TestTree
makeVanillaTest = makeTestWith S.empty

makeTestWith :: S.Set VariableName -> Text -> Text -> TestTree
makeTestWith bindings input expected =
    testCase (unpack $ deline input) $ case (,) <$> parseModule (unpack input) <*> parseModule (unpack expected) of
        ParseOk (input', expected') ->
            case runExcept renamedInput of
                Right (actual, _) -> do
                    let expected'' = stripModuleParens expected'
                    assertAlphaEq (unlines [showt actual, showt expected'', "", synPrint actual, synPrint expected'', "", pretty state, unlines logs]) expected'' actual
                Left err     -> assertFailure $ unpack $ unlines [err, pretty state, unlines logs]
            where ((renamedInput, state), logs) = evalNameGenerator (runLoggerT $ runRenamer $ bindVariableForScope bindings $ renameModule input') 0
        ParseFailed loc msg -> assertFailure $ unpack $ "Failed to parse input: " <> showt loc <> "\n" <> pack msg

test :: TestTree
test = testGroup "Renamer"
    [ makeTest "x = 5" "z0 = 5"
    , makeTest
        "x = 5 ; y = x"
        "z0 = 5 ; z1 = z0"
    , makeTest
        "x = 1 + 2"
        "z0 = 1 `z1` 2"
    , makeTest
        "x = y ; y = x"
        "z0 = z1 ; z1 = z0"
    , makeTest
        "(a, b) = (x, 4) ; x = 1"
        "T z0 z1 = T z2 4 ; z2 = 1"
    , makeTest
        "(_, a) = 4"
        "T _ z0 = 4"
    , makeTest
        "x@(a, b) = (1, 2)"
        "z2@(T z0 z1) = T 1 2"
    , makeTest
        "x = 2 ; [a, 2, c] = [1, x, 3]"
        "z2 = 2 ; L z0 2 z1 = L 1 z2 3"
    , makeTest
        "_ = let { x = 0 ; y = 1 ; z = 2 } in if x then y else z"
        "_ = let { z0 = 0 ; z1 = 1 ; z2 = 2 } in if z0 then z1 else z2"
    , makeTest
        "_ = \\x -> x"
        "_ = \\z0 -> z0"
    , makeTest
        "_ = let { x = 0 ; y = (\\x -> x) } in y"
        "_ = let { z0 = 0 ; z1 = (\\z2 -> z2) } in z1"
    , makeTest
        "f = (\\x -> x) :: a -> a ; g = f :: a -> a"
        "f = (\\x -> x) :: z0 -> z0 ; g = f :: z1 -> z1"
    , makeVanillaTest
        "class Foo a where { bar :: Int -> a -> a }"
        "class Foo z0 where { z1 :: Int -> z0 -> z0 }"
    , makeTest
        "data Foo a b = Foo a b ; f = \\x (Foo y z@(w, u)) -> (y, u)"
        "data Foo a b = Foo a b ; f = \\z0 (Foo z1 z2@(T z3 z4)) -> T z1 z4"
    , makeTest
        "f x = x"
        "z0 z1 = z1"
    , makeVanillaTest
        "data Bool = True | False ; f True x 0 = x"
        "data Bool = True | False ; z0 True z2 0 = z2"
    , makeVanillaTest
        "data Bool = False | True ; data Maybe a = Nothing | Just a ; f = \\x -> case x of { Nothing -> False ; Just x -> x }"
        "data Bool = False | True ; data Maybe a = Nothing | Just a ; f = \\z0 -> case z0 of { Nothing -> False ; Just z1 -> z1 }"
    , makeVanillaTest
        "data Bool = True | False ; data [] a = E | C a [a] ; f = \\x -> x ; y = f (C True (C False E))"
        "data Bool = True | False ; data [] a = V0 | V1 a [a] ; f = \\x -> x ; y = f (V1 True (V1 False V0))"
    , makeVanillaTest
        "data Bool = True | False ; data [] a = [] | a :+ [a] ; f = \\x -> x ; y = f (True:+(False:+[]))"
        "data Bool = True | False ; data [] a = V0 | V1 a [a] ; f = \\x -> x ; y = f (True `V1` (False `V1` V0))"
    , makeVanillaTest
        "data [] a = [] | a :+ [a] ; x = []"
        "data [] a = V0 | V1 a [a] ; x = V0"
    , makeVanillaTest
        "data [] a = [] | a :+ [a] ; x = []:+[]"
        "data [] a = V0 | V1 a [a] ; x = V0 `V1` V0"
    ]
