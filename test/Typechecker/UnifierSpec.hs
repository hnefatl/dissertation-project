{-# LANGUAGE TupleSections #-}

module Typechecker.UnifierSpec where

import           BasicPrelude
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.HUnit         (assertEqual, testCase)

import           Data.Text                (unpack)
import           Names
import           TextShow                 (showt)
import           Typechecker.Substitution (Substitution(..), TypeSubstitution, subMultiple)
import           Typechecker.Types
import           Typechecker.Unifier      (match, mgu)

import qualified Data.Map                 as M

test :: TestTree
test =
    let
        [a, b, c] = map TypeVariableName ["a", "b", "c"]
        [ta, tb, tc] = [TypeVar (TypeVariable v KindStar) | v <- [a, b, c]]
    in testGroup "Unification"
    [
        let -- x = [(a, (Int, c))]
            x = makeList (makeTuple [ta, makeTuple [typeInt, tc]])
            -- y = [(Char -> c, (b, Bool))]
            y = makeList (makeTuple [makeFun [typeChar] tc, makeTuple [tb, typeBool]])

            actual = mgu x y :: Either Text TypeSubstitution
            -- expected = [(Char -> Bool)/a, Int/b, Bool/c]
            expected = Right $ subMultiple [(a, makeFun [typeChar] typeBool), (b, typeInt), (c, typeBool)]

        in testCase (unpack $ "mgu (" <> showt x <> ") (" <> showt y <> ")") $ assertEqual "" expected actual
    ,
        let x = IsInstance (TypeVariableName "Eq") (TypeVar (TypeVariable a KindStar))
            y = IsInstance (TypeVariableName "Eq") typeBool
        in testCase "match (Eq a) (Eq Bool)" $ assertEqual "" (Right $ subMultiple [(a, typeBool)]) (match x y)
    ,
        let expected = Substitution $ M.fromList $ map (,typeBool) [a,b,c]
            actual = mgu (makeFun [makeFun [typeBool] ta] ta) (makeFun [makeFun [tb] tb] tc)
        in testCase "mgu ((Bool -> a) -> a) ((b -> b) -> c)" $ assertEqual "" (Right expected) actual
    ]
