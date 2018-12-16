module Backend.DeoverloadSpec where

import Test.Tasty
import Test.Tasty.HUnit

import AlphaEq
import ExtraDefs
import NameGenerator
import Typechecker.Types
import Typechecker.Typechecker
import Backend.Deoverload

import Control.Monad.Except
import Language.Haskell.Parser
import Data.Text.Lazy (unpack)
import Text.Printf
import Text.Pretty.Simple

makeTest :: String -> String -> TestTree
makeTest sExpected sActual = testCase (deline sActual) $
    case (parseModule sExpected, parseModule sActual) of
        (ParseOk expected, ParseOk actualModule) ->
            case runExcept x of
                Left err -> assertFailure $ printf "Failed to deoverload: %s\n%s" err (unpack $ pShow state)
                Right actual -> assertBool (show actual) (alphaEq expected actual)
            where (x, state) = evalNameGenerator (runDeoverload $ deoverloadModule actualModule) 0
        (ParseFailed _ _, _) -> assertFailure "Failed to parse expected"
        (_, ParseFailed _ _) -> assertFailure "Failed to parse actual"

test :: TestTree
test = testGroup "Deoverload"
    [ makeTest "f = \\x -> x + x" "f = \\d -> \\x -> (+) d x x" ]