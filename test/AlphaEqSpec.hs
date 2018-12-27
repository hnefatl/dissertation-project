module AlphaEqSpec where

import Test.Tasty
import Test.Tasty.HUnit

import BasicPrelude
import TextShow (TextShow, showt)
import Control.Monad.Extra (whenJust)
import Data.Text (unpack)
import Language.Haskell.Syntax
import qualified Data.Set as S

import AlphaEq
import Names
import Typechecker.Types

makeTest :: (TextShow a, AlphaEq a) => a -> a -> TestTree
makeTest x y = testCase (unpack $ showt x <> " vs " <> showt y) $
    whenJust result (\err -> assertFailure $ unpack $ unlines [err, showt state])
    where (result, state) = runAlphaEq x y
makeFailTest :: (TextShow a, AlphaEq a) => a -> a -> TestTree
makeFailTest x y = testCase (unpack $ "Fails: " <> showt x <> " vs " <> showt y) $
    when (isNothing result) (assertFailure $ show state)
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
    , makeFailTest
        (Qualified (S.singleton $ num ta) $ makeFun [ta] ta)
        (Qualified S.empty $ makeFun [ta] ta)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num tc, fractional td]) $ makeFun [tc] td)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFun [td] tc)
    , makeFailTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFun [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFun [tc] td)
    , makeFailTest
        (Quantified (S.singleton a) $ Qualified (S.singleton $ num ta) $ makeFun [ta] ta)
        (Quantified (S.singleton a) $ Qualified S.empty $ makeFun [ta] ta)
    , makeTest
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "a"])] $ HsTyVar $ HsIdent "a")
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "b"])] $ HsTyVar $ HsIdent "b")
    , makeFailTest
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "a"])] $ HsTyVar $ HsIdent "a")
        (HsQualType [] $ HsTyVar $ HsIdent "a")
    ]
    where
        [a, b, c, d] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b", "c", "d"]
        [ta, tb, tc, td] = map TypeVar [a, b, c, d]
        num = IsInstance (TypeVariableName "Num")
        fractional = IsInstance (TypeVariableName "Fractional")