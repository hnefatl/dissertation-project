module Typechecker.SubstitutionSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Substitution"
    [ let [a, b, c] = [TypeVariable name KindStar | name <- ["a", "b", "c"]]
          -- x = [Int/a, Bool/b]
          x = subMultiple [(a, typeInt), (b, typeBool)]
          -- y = [(a -> b)/c]
          y = subSingle c (makeFun (TypeVar a) (TypeVar b))

          actual = subCompose x y
          -- expected = [Int/a, Bool/b, (a -> b)/c]
          expected = subMultiple [(a, typeInt), (b, typeBool), (c, makeFun (TypeVar a) (TypeVar b))]

      in testCase ("subCompose " ++ assocShow True x ++ " " ++ assocShow True y) $ assertEqual "" expected actual
    ]