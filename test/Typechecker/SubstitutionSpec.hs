module Typechecker.SubstitutionSpec where

import BasicPrelude
import Data.Text                (unpack)
import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.HUnit         (assertEqual, testCase)
import TextShow                 (showt)

import Names
import Typechecker.Substitution
import Typechecker.Types

test :: TestTree
test = testGroup "Substitution"
    [
        let [a, b, c] = map TypeVariableName ["a", "b", "c"]
            -- x = [(a -> b)/c]
            x = subSingle c (makeFunUnsafe [TypeVar (TypeVariable a KindStar)] (TypeVar (TypeVariable b KindStar)))
            -- y = [Int/a, Bool/b]
            y = subMultiple [(a, typeInt), (b, typeBool)]

            actual = subCompose x y
            -- expected = [Int/a, Bool/b, (Int -> Bool)/c]
            expected = subMultiple [(a, typeInt), (b, typeBool), (c, makeFunUnsafe [typeInt] typeBool)]
        in testCase (unpack $ "subCompose (" <> showt x <> ") (" <> showt y <> ")") $ assertEqual "" expected actual
    ]
