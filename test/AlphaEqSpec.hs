module AlphaEqSpec where

import           Test.Tasty
import           Test.Tasty.HUnit

import           BasicPrelude
import           Control.Monad.Extra     (whenJust)
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           Language.Haskell.Pretty (Pretty, prettyPrint)
import           Language.Haskell.Syntax
import           TextShow                (TextShow, showt)

import           AlphaEq
import           Names
import           Typechecker.Types

synPrint :: Pretty a => a -> Text
synPrint = pack . prettyPrint

makeTest :: (TextShow a, AlphaEq a) => a -> a -> TestTree
makeTest = makeTestWith showt
makeTestWith :: AlphaEq a => (a -> Text) -> a -> a -> TestTree
makeTestWith f x y = testCase (unpack $ f x <> " vs " <> f y) $
    whenJust result (\err -> assertFailure $ unpack $ unlines [err, showt state])
    where (result, state) = runAlphaEq x y
makeFailTest :: (TextShow a, AlphaEq a) => a -> a -> TestTree
makeFailTest = makeFailTestWith showt
makeFailTestWith :: AlphaEq a => (a -> Text) -> a -> a -> TestTree
makeFailTestWith f x y = testCase (unpack $ "Fails: " <> f x <> " vs " <> f y) $
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
        (Qualified (S.fromList [num ta, num tb]) $ makeFunUnsafe [ta] tb)
        (Qualified (S.fromList [num td, num tc]) $ makeFunUnsafe [td] tc)
    , makeTest
        (Qualified (S.fromList [num ta, fractional tb]) $ makeFunUnsafe [ta] tb)
        (Qualified (S.fromList [num tc, fractional td]) $ makeFunUnsafe [tc] td)
    , makeFailTest
        (Qualified (S.fromList [num ta, fractional tb]) $ makeFunUnsafe [ta] tb)
        (Qualified (S.fromList [num tc, fractional td]) $ makeFunUnsafe [td] tc)
    , makeFailTest
        (Qualified (S.singleton $ num ta) $ makeFunUnsafe [ta] ta)
        (Qualified S.empty $ makeFunUnsafe [ta] ta)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFunUnsafe [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num tc, fractional td]) $ makeFunUnsafe [tc] td)
    , makeTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFunUnsafe [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFunUnsafe [td] tc)
    , makeFailTest
        (Quantified (S.fromList [a, b]) $ Qualified (S.fromList [num ta, fractional tb]) $ makeFunUnsafe [ta] tb)
        (Quantified (S.fromList [c, d]) $ Qualified (S.fromList [num td, fractional tc]) $ makeFunUnsafe [tc] td)
    , makeFailTest
        (Quantified (S.singleton a) $ Qualified (S.singleton $ num ta) $ makeFunUnsafe [ta] ta)
        (Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [ta] ta)
    , makeTestWith synPrint
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "a"])] $ HsTyVar $ HsIdent "a")
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "b"])] $ HsTyVar $ HsIdent "b")
    , makeFailTestWith synPrint
        (HsQualType [(UnQual $ HsIdent "Num", [HsTyVar $ HsIdent "a"])] $ HsTyVar $ HsIdent "a")
        (HsQualType [] $ HsTyVar $ HsIdent "a")
    ]
    where
        [a, b, c, d] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b", "c", "d"]
        [ta, tb, tc, td] = map TypeVar [a, b, c, d]
        num = IsInstance (TypeVariableName "Num")
        fractional = IsInstance (TypeVariableName "Fractional")
