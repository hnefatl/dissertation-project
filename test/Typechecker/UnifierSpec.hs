module Typechecker.UnifierSpec where

import Test.Tasty
import Test.Tasty.HUnit

import ExtraDefs
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Types


test :: TestTree
test = let
        [a, b, c] = map Id ["a", "b", "c"]
        [ta, tb, tc] = [TypeVar (TypeVariable v KindStar) | v <- [a, b, c]]
    in testGroup "Unification"
    [
        let -- x = [(a, (Int, c))]
            x = makeList (makeTuple [ta, makeTuple [typeInt, tc]])
            -- y = [(Char -> c, (b, Bool))]
            y = makeList (makeTuple [makeFun [typeChar] tc, makeTuple [tb, typeBool]])

            actual = mgu x y :: Either String Substitution
            -- expected = [(Char -> Bool)/a, Int/b, Bool/c]
            expected = Right $ subMultiple [(a, makeFun [typeChar] typeBool), (b, typeInt), (c, typeBool)]

        in testCase ("mgu (" ++ show x ++ ") (" ++ show y ++ ")") $ assertEqual "" expected actual
    ,
        let x = IsInstance (Id "Eq") (TypeVar (TypeVariable a KindStar))
            y = IsInstance (Id "Eq") typeBool
        in testCase "match (Eq a) (Eq Bool)" $ assertEqual "" (Right $ subMultiple [(a, typeBool)]) (match x y)
    ]