module Typechecker.UnifierSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Unification"
    [
        let [a, b, c] = [TypeVariable name KindStar | name <- ["a", "b", "c"]]
            [ta, tb, tc] = [TypeVar v | v <- [a, b, c]]

            -- x = [(a, (Int, c))]
            x = makeList (makeTuple [ta, makeTuple [typeInt, tc]])
            -- y = [(Char -> c, (b, Bool))]
            y = makeList (makeTuple [makeFun [typeChar] tc, makeTuple [tb, typeBool]])

            actual = mgu x y :: Either String Substitution
            -- expected = [(Char -> Bool)/a, Int/b, Bool/c]
            expected = Right $ subMultiple [(a, makeFun [typeChar] typeBool), (b, typeInt), (c, typeBool)]

        in testCase ("mgu (" ++ show x ++ ") (" ++ show y ++ ")") $ assertEqual "" expected actual
    ,
        let a = TypeVariable "a" KindStar
            x = IsInstance "Eq" (TypeVar a)
            y = IsInstance "Eq" typeBool
        in testCase "match (Eq a) (Eq Bool)" $ assertEqual "" (Right $ subMultiple [(a, typeBool)]) (match x y)
    ,
        let [a, b, c, d] = map (\name -> TypeVariable name KindStar) ["a", "b", "c", "d"]
            x = makeFun [makeFun [a] typeBool] b
            y = makeFun [makeFun [c] d] d
            expected = Right $ subMultiple [(a, c), (d, typeBool), (b, typeBool)]
            actual = mgu x y
        in testCase ("mgu (" ++ show x ++ ") (" ++ show y ++")") $ assertEqual "" expected actual
    ]