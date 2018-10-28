module Typechecker.SubstitutionSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Substitution"
    [
        let [a, b, c] = [TypeVariable name KindStar | name <- ["a", "b", "c"]]
            -- x = [(a -> b)/c]
            x = subSingle c (makeFun [TypeVar a] (TypeVar b))
            -- y = [Int/a, Bool/b]
            y = subMultiple [(a, typeInt), (b, typeBool)]

            actual = subCompose x y
            -- expected = [Int/a, Bool/b, (Int -> Bool)/c]
            expected = subMultiple [(a, typeInt), (b, typeBool), (c, makeFun [typeInt] typeBool)]
        in testCase ("subCompose (" ++ show x ++ ") (" ++ show y ++ ")") $ assertEqual "" expected actual
    ]