module Backend.DeoverloadSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Parser

import Control.Monad.Except
import Data.Text.Lazy (unpack)
import Text.Pretty.Simple

import AlphaEq
import ExtraDefs
import NameGenerator
import Typechecker.Typechecker
import Backend.Deoverload

pretty :: Show a => a -> String
pretty = unpack . pShow

makeTest :: String -> String -> TestTree
makeTest sActual sExpected = testCase (deline sActual) $
    case (parseModule sExpected, parseModule sActual) of
        (ParseOk expected, ParseOk actualModule) -> do
            let infer = runTypeInferrer $ inferModuleWithBuiltins actualModule
                ((eTypes, tState), i) = runNameGenerator infer 0
            case runExcept eTypes of
                Left err -> assertFailure err
                Right (_, ts) -> do
                    let deoverload = runDeoverload $ addTypes ts >> deoverloadModule actualModule
                        (eDeoverloaded, dState) = evalNameGenerator deoverload i
                    case runExcept eDeoverloaded of
                        Left err -> assertFailure $ unlines [err, pretty tState, pretty dState]
                        Right actual -> assertBool (show actual) (alphaEq expected actual)
        (ParseFailed _ _, _) -> assertFailure "Failed to parse expected"
        (_, ParseFailed _ _) -> assertFailure "Failed to parse actual"

test :: TestTree
test = testGroup "Deoverload"
    []
    -- TODO(kc506): Yeah... Get the deoverloader to use the right types.
    --[ makeTest "f = \\x -> x + x" "f = \\d -> \\x -> (+) d x x" ]