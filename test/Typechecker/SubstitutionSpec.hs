module Typechecker.SubstitutionSpec where

import Test.Tasty
import Test.Tasty.HUnit

import ExtraDefs
import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Substitution"
    [
        let [a, b, c] = map Id ["a", "b", "c"]
            -- x = [(a -> b)/c]
            x = subSingle c (makeFun [TypeVar (TypeVariable a KindStar)] (TypeVar (TypeVariable b KindStar)))
            -- y = [Int/a, Bool/b]
            y = subMultiple [(a, typeInt), (b, typeBool)]

            actual = subCompose x y
            -- expected = [Int/a, Bool/b, (Int -> Bool)/c]
            expected = subMultiple [(a, typeInt), (b, typeBool), (c, makeFun [typeInt] typeBool)]
        in testCase ("subCompose (" ++ show x ++ ") (" ++ show y ++ ")") $ assertEqual "" expected actual
    ]