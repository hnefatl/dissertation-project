module AlphaEqSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Text.Printf
import qualified Data.Set as S

import AlphaEq
import Names
import Typechecker.Types

makeTest :: (Show a, AlphaEq a) => a -> a -> TestTree
makeTest x y = testCase (printf "%s vs %s" (show x) (show y)) $ unless result (assertFailure $ show state)
    where (result, state) = runAlphaEq x y
makeFailTest :: (Show a, AlphaEq a) => a -> a -> TestTree
makeFailTest x y = testCase (printf "Fails: %s vs %s" (show x) (show y)) $ when result (assertFailure $ show state)
    where (result, state) = runAlphaEq x y

test :: TestTree
test = testGroup "AlphaEq"
    [ makeTest a a
    , makeTest a b
    , makeFailTest a (TypeVariable (TypeVariableName "a") $ KindFun KindStar KindStar)
    , makeTest (S.fromList [a, b]) (S.fromList [c, d])
    , makeFailTest (S.fromList [a, b]) S.empty
    , makeFailTest (S.fromList [a, b]) (S.singleton c)
    , makeTest (Qualified (S.singleton $ num ta) ta) (Qualified (S.singleton $ num tb) tb)
    , makeTest
        (Qualified (S.fromList [num ta, num tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [num td, num tc]) $ makeFun [td] tc)
    , makeTest
        (Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [num tc, fractional td]) $ makeFun [tc] td)
    , makeFailTest
        (Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Qualified (S.fromList [num tc, fractional td]) $ makeFun [td] tc)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num tc, fractional td]) $ makeFun [tc] td)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFun [td] tc)
    , makeFailTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFun [tc] td)
    ]
    where
        [a, b, c, d] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b", "c", "d"]
        [ta, tb, tc, td] = map TypeVar [a, b, c, d]
        num = IsInstance (TypeConstantName "Num")
        fractional = IsInstance (TypeConstantName "Fractional")