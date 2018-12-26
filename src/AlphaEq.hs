{-# Language GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Haskell.Syntax
import Language.Haskell.Pretty
import Data.Either
import Text.Printf

import ExtraDefs
import Names
import Typechecker.Types

newtype AlphaEqM a = AlphaEqM { inner :: ExceptT String (State (M.Map String String)) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String String), MonadError String)

class Show a => AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM ()
alphaEqBool' :: AlphaEq a => a -> a -> AlphaEqM Bool
alphaEqBool' x y = catchError (alphaEq' x y >> return True) (const $ return False)
alphaEqs' :: AlphaEq a => [a] -> [a] -> AlphaEqM ()
alphaEqs' [] [] = return ()
alphaEqs' (x:xs) (y:ys) = alphaEq' x y >> alphaEqs' xs ys
alphaEqs' xs ys = throwError $ printf "List length mismatch: %s\nvs\n%s" (show xs) (show ys)

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = isRight $ alphaEqError x y
alphaEqError :: (MonadError String m, AlphaEq a) => a -> a -> m ()
alphaEqError x y = liftEither $ evalState (runExceptT $ inner $ alphaEq' x y) M.empty

runAlphaEq :: AlphaEq a => a -> a -> (Maybe String, M.Map String String)
runAlphaEq x y = (either Just (const Nothing) result, s)
    where (result, s) = runState (runExceptT $ inner $ alphaEq' x y) M.empty

instance AlphaEq String where
    alphaEq' s1 s2
        | s1 == s2 = return ()
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) ->
                unless (x == s2 && y == s1) $ throwError $ printf "%s and %s have already been renamed" s1 s2
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (Nothing, _) -> throwError $ printf "%s has been renamed but %s hasn't" s1 s2
            (_, Nothing) -> throwError $ printf "%s has been renamed but %s hasn't" s2 s1
-- TODO(kc506): When we swap String for Text, add an instance for lists and find all the places we can use that.
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return () -- Both empty, both alpha equivalent
        | S.null s1 = throwError $ printf "Set is non-empty: %s" (show s2)
        | S.null s2 = throwError $ printf "Set is non-empty: %s" (show s1)
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            findM (alphaEqBool' x) (S.toList s2) >>= \case -- Find an alpha-eq element from the other set
                Nothing -> throwError $ printf "Couldn't find alpha-eq element for %s in %s" (show x) (show s2)
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse


instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2
instance AlphaEq TypeVariable where
    alphaEq' (TypeVariable n1 k1) (TypeVariable n2 k2) = do
        unless (k1 == k2) $ throwError $ printf "Kind mismatch: %s vs %s" (show n1) (show n2)
        alphaEq' n1 n2
instance AlphaEq TypeConstant where
    alphaEq' (TypeConstant n1 k1) (TypeConstant n2 k2) = do
        unless (n1 == n2) $ throwError $ printf "Name mismatch: %s vs %s" (show n1) (show n2)
        unless (k1 == k2) $ throwError $ printf "Kind mismatch: %s vs %s" (show n1) (show n2)
instance AlphaEq Type where
    alphaEq' (TypeVar t1) (TypeVar t2) = alphaEq' t1 t2
    alphaEq' (TypeCon c1) (TypeCon c2) = alphaEq' c1 c2
    alphaEq' (TypeApp t1a t1b k1) (TypeApp t2a t2b k2) = do
        unless (k1 == k2) $ throwError $ printf "Kind mismatch: %s vs %s" (show k1) (show k2)
        alphaEq' t1a t2a
        alphaEq' t1b t2b
    alphaEq' t1 t2 = throwError $ printf "Different types: %s vs %s" (show t1) (show t2)
instance AlphaEq TypePredicate where
    alphaEq' (IsInstance c1 t1) (IsInstance c2 t2) = do
        unless (c1 == c2) $ throwError $ printf "Class mismatch: %s vs %s" (show c1) (show t1)
        alphaEq' t1 t2
instance AlphaEq a => AlphaEq (Qualified a) where
    alphaEq' (Qualified quals1 t1) (Qualified quals2 t2) = alphaEq' t1 t2 >> alphaEq' quals1 quals2
instance AlphaEq QuantifiedType where
    alphaEq' (Quantified quants1 t1) (Quantified quants2 t2) = alphaEq' t1 t2 >> alphaEq' quants1 quants2

instance AlphaEq HsModule where
    alphaEq' (HsModule _ _ _ _ ds1) (HsModule _ _ _ _ ds2) = alphaEqs' ds1 ds2
instance AlphaEq HsDecl where
    alphaEq' (HsPatBind _ pat1 rhs1 ds1) (HsPatBind _ pat2 rhs2 ds2) = do
        alphaEq' pat1 pat2
        alphaEq' rhs1 rhs2
        alphaEqs' ds1 ds2
    alphaEq' d1 d2 = throwError $ printf "Different declaration types:\n%s\nvs\n%s" (show d1) (show d2)
instance AlphaEq HsPat where
    alphaEq' (HsPVar n1) (HsPVar n2) = alphaEq' (convertName n1 :: String) (convertName n2)
    alphaEq' (HsPLit l1) (HsPLit l2) =
        unless (l1 == l2) $ throwError $ printf "Literal pat mismatch:\n%s\nvs\n%s" (prettyPrint l1) (prettyPrint l2)
    alphaEq' (HsPApp con1 ps1) (HsPApp con2 ps2) = do
        alphaEq' (convertName con1 :: String) (convertName con2) 
        alphaEqs' ps1 ps2
    alphaEq' (HsPTuple ps1) (HsPTuple ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPList ps1) (HsPList ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPParen p1) (HsPParen p2) = alphaEq' p1 p2
    alphaEq' (HsPAsPat v1 p1) (HsPAsPat v2 p2) = do
        alphaEq' (convertName v1 :: String) (convertName v2)
        alphaEq' p1 p2
    alphaEq' HsPWildCard HsPWildCard = return ()
    alphaEq' p1 p2 = throwError $ printf "Pattern mismatch:\n%s\nvs\n%s" (prettyPrint p1) (prettyPrint p2)
instance AlphaEq HsRhs where
    alphaEq' (HsUnGuardedRhs e1) (HsUnGuardedRhs e2) = alphaEq' e1 e2
    alphaEq' r1 r2 = throwError $ printf "RHS mismatch: %s\nvs\n%s" (prettyPrint r1) (prettyPrint r2)
instance AlphaEq HsExp where
    alphaEq' (HsVar v1) (HsVar v2) = alphaEq' (convertName v1 :: String) (convertName v2)
    alphaEq' (HsCon c1) (HsCon c2) = alphaEq' (HsVar c1) (HsVar c2)
    alphaEq' (HsLit l1) (HsLit l2) =
        unless (l1 == l2) $ throwError $ printf "Literal exp mismatch: %s vs %s" (prettyPrint l1) (prettyPrint l2)
    alphaEq' (HsApp e1a e1b) (HsApp e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (HsNegApp e1) (HsNegApp e2) = alphaEq' e1 e2
    alphaEq' (HsLambda _ ps1 e1) (HsLambda _ ps2 e2) = alphaEqs' ps1 ps2 >> alphaEq' e1 e2
    alphaEq' (HsLet ds1 e1) (HsLet ds2 e2) = alphaEqs' ds1 ds2 >> alphaEq' e1 e2
    alphaEq' (HsIf e1a e1b e1c) (HsIf e2a e2b e2c) = alphaEqs' [e1a, e1b, e1c] [e2a, e2b, e2c]
    alphaEq' (HsTuple es1) (HsTuple es2) = alphaEqs' es1 es2
    alphaEq' (HsList es1) (HsList es2) = alphaEqs' es1 es2
    alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    alphaEq' (HsExpTypeSig _ e1 t1) (HsExpTypeSig _ e2 t2) = alphaEq' e1 e2 >> alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ printf "Expression mismatch:\n%s\nvs\n%s" (show e1) (show e2)
instance AlphaEq HsType where
    alphaEq' (HsTyFun t1a t1b) (HsTyFun t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyTuple ts1) (HsTyTuple ts2) = alphaEqs' ts1 ts2
    alphaEq' (HsTyApp t1a t1b) (HsTyApp t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyVar v1) (HsTyVar v2) = alphaEq' (convertName v1 :: String) (convertName v2)
    alphaEq' (HsTyCon v1) (HsTyCon v2) =
        unless (v1 == v2) $ throwError $ printf "Name mismatch: %s vs %s" (prettyPrint v1) (prettyPrint v2)
    alphaEq' t1 t2 = throwError $ printf "Type mismatch: %s vs %s" (prettyPrint t1) (prettyPrint t2)
instance AlphaEq HsQualType where
    alphaEq' (HsQualType c1 t1) (HsQualType c2 t2) = alphaEqs' c1 c2 >> alphaEq' t1 t2
instance AlphaEq HsAsst where
    alphaEq' (name1, ts1) (name2, ts2) = do
        unless (name1 == name2) $ throwError $ printf "Name mismatch: %s vs %s" (prettyPrint name1) (prettyPrint name2)
        alphaEqs' ts1 ts2

stripModuleParens :: HsModule -> HsModule
stripModuleParens (HsModule a b c d e) = HsModule a b c d (stripDeclsParens e)
stripDeclParens :: HsDecl -> HsDecl
stripDeclParens (HsPatBind l p r ds) = HsPatBind l p (stripRhsParens r) (stripDeclsParens ds)
stripDeclParens _ = error "Unsupported declaration in paren strip"
stripDeclsParens :: [HsDecl] -> [HsDecl]
stripDeclsParens = map stripDeclParens
stripRhsParens :: HsRhs -> HsRhs
stripRhsParens (HsUnGuardedRhs e) = HsUnGuardedRhs (stripExpParens e)
stripRhsParens _ = error "Unsupported RHS in paren strip"
stripExpParens :: HsExp -> HsExp
stripExpParens (HsParen e) = stripExpParens e
stripExpParens (HsApp e1 e2) = HsApp (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsInfixApp e1 op e2) = HsInfixApp (stripExpParens e1) op (stripExpParens e2)
stripExpParens (HsNegApp e) = HsNegApp (stripExpParens e)
stripExpParens (HsLambda l ps e) = HsLambda l ps (stripExpParens e)
stripExpParens (HsIf c e1 e2) = HsIf (stripExpParens c) (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsLet ds e) = HsLet ds (stripExpParens e)
stripExpParens (HsTuple es) = HsTuple (map stripExpParens es)
stripExpParens (HsList es) = HsList (map stripExpParens es)
stripExpParens (HsExpTypeSig l e t) = HsExpTypeSig l (stripExpParens e) t
stripExpParens e = e