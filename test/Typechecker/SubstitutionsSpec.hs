module Typechecker.SubstitutionsSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Substitutions
import Typechecker.Types

test :: TestTree
test = testGroup "Substitutions"
    [ let a = TypeVariable "a" KindStar
          b = TypeVariable "b" KindStar
          c = TypeVariable "c" (KindFun KindStar KindStar)
          -- x = [Int/a, Bool/b]
          x = subMultiple [(a, typeInt), (b, typeBool)]
          -- y = [(a -> b)/c]
          y = subSingle c (makeFun (TypeVar a) (TypeVar b))

          actual = subCompose x y
          -- expected = [Int/a, Bool/b, (Int -> Bool)/c]
          expected = subMultiple [(a, typeInt), (b, typeBool), (c, makeFun typeInt typeBool)]

      in testCase "[Int/a, Bool/b] . [(a -> b)/c]" $ assertEqual "" expected actual
    ]