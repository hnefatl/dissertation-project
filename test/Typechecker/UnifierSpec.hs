module Typechecker.UnifierSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Types



test :: TestTree
test = let
        [a, b, c, d] = ["a", "b", "c", "d"]
        [ta, tb, tc, td] = [TypeVar (TypeVariable v KindStar) | v <- [a, b, c, d]]
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
        let x = IsInstance "Eq" (TypeVar (TypeVariable a KindStar))
            y = IsInstance "Eq" typeBool
        in testCase "match (Eq a) (Eq Bool)" $ assertEqual "" (Right $ subMultiple [(a, typeBool)]) (match x y)
    ,
        let x = makeFun [makeFun [ta] typeBool] tb
            y = makeFun [makeFun [tc] td] td
            expected = Right $ subMultiple [(a, tc), (d, typeBool), (b, typeBool)]
            actual = mgu x y
        in testCase ("mgu (" ++ show x ++ ") (" ++ show y ++")") $ assertEqual "" expected actual
    ]