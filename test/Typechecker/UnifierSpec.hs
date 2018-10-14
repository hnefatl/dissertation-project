module Typechecker.UnifierSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Unification"
    [ let [a, b, c] = [TypeVariable name KindStar | name <- ["a", "b", "c"]]
          [ta, tb, tc] = [TypeVar v | v <- [a, b, c]]

          -- x = [(a, (Int, c))]
          x = makeList (makeTuple2 ta (makeTuple2 typeInt tc))
          -- y = [(Char -> c, (b, Bool))]
          y = makeList (makeTuple2 (makeFun typeChar tc) (makeTuple2 tb typeBool))

          actual = mgu x y :: Either String Substitution
          -- expected = [(Char -> Bool)/a, Int/b, Bool/c]
          expected = Right $ subMultiple [(a, makeFun typeChar typeBool), (b, typeInt), (c, typeBool)]

      in testCase ("mgu (" ++ show x ++ ") (" ++ show y ++ ")") $ assertEqual "" expected actual
    ]