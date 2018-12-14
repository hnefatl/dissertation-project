module AlphaEqSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Text.Printf
import qualified Data.Set as S

import AlphaEq
import Names
import Typechecker.Types

makeTest :: (Show a, Show k, AlphaEq k a) => a -> a -> TestTree
makeTest x y = testCase (printf "%s vs %s" (show x) (show y)) $ unless result (assertFailure $ show state)
    where (result, state) = runAlphaEq x y
makeFailTest :: (Show a, Show k, AlphaEq k a) => a -> a -> TestTree
makeFailTest x y = testCase (printf "Fails: %s vs %s" (show x) (show y)) $ when result (assertFailure $ show state)
    where (result, state) = runAlphaEq x y

test :: TestTree
test = testGroup "AlphaEq"
    [ makeTest a a
    , makeTest a b
    , makeFailTest a (TypeVariable (TypeVariableName "a") $ KindFun KindStar KindStar)
    , makeTest (Qualified (S.singleton $ IsInstance num ta) ta) (Qualified (S.singleton $ IsInstance num tb) tb)
    , makeTest
        (Qualified (S.fromList [IsInstance num ta, IsInstance num tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [IsInstance num td, IsInstance num tc]) $ makeFun [td] tc)
    , makeTest
        (Qualified (S.fromList [IsInstance num ta, IsInstance fractional tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [IsInstance num tc, IsInstance fractional td]) $ makeFun [tc] td)
    , makeFailTest
        (Qualified (S.fromList [IsInstance num ta, IsInstance fractional tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [IsInstance num tc, IsInstance fractional td]) $ makeFun [td] tc)
    ]
    where
        [a, b, c, d] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b", "c", "d"]
        [ta, tb, tc, td] = map TypeVar [a, b, c, d]
        num = TypeConstantName "Num"
        fractional = TypeConstantName "Fractional"