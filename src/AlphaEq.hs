{-# Language GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Haskell.Syntax

import ExtraDefs
import Names
import Typechecker.Types

newtype AlphaEqM a = AlphaEqM { inner :: State (M.Map String String) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String String))

class AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM Bool
alphaEqs' :: AlphaEq a => [a] -> [a] -> AlphaEqM Bool
alphaEqs' [] [] = return True
alphaEqs' (x:xs) (y:ys) = (&&) <$> alphaEq' x y <*> alphaEqs' xs ys
alphaEqs' _ _ = return False

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = evalState (inner $ alphaEq' x y) M.empty

runAlphaEq :: AlphaEq a => a -> a -> (Bool, M.Map String String)
runAlphaEq x y = runState (inner $ alphaEq' x y) M.empty

instance AlphaEq String where
    alphaEq' s1 s2
        | s1 == s2 = return True
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> do
                modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
                return True
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) -> return $ x == s2 && y == s1
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (_, _) -> return False
-- TODO(kc506): When we swap String for Text, add an instance for lists and find all the places we can use that.
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return True -- Both empty, both alpha equivalent
        | S.null s1 || S.null s2 = return False -- Different sizes, can't be alpha equivalent
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            findM (alphaEq' x) (S.toList s2) >>= \case -- Find an alpha-eq element from the other set
                Nothing -> return False -- No matching element
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse


instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2
instance AlphaEq TypeVariable where
    alphaEq' (TypeVariable n1 k1) (TypeVariable n2 k2) = (k1 == k2 &&) <$> alphaEq' n1 n2
instance AlphaEq Type where
    alphaEq' (TypeVar t1) (TypeVar t2) = alphaEq' t1 t2
    alphaEq' (TypeConstant n1 ks1 ts1) (TypeConstant n2 ks2 ts2) = do
        tsOkay <- and <$> zipWithM alphaEq' ts1 ts2
        return $ n1 == n2 && ks1 == ks2 && tsOkay
    alphaEq' _ _ = return False
instance AlphaEq TypePredicate where
    alphaEq' (IsInstance c1 t1) (IsInstance c2 t2) = (c1 == c2 &&) <$> alphaEq' t1 t2
instance AlphaEq a => AlphaEq (Qualified a) where
    alphaEq' (Qualified quals1 t1) (Qualified quals2 t2) = (&&) <$> alphaEq' t1 t2 <*> alphaEq' quals1 quals2
instance AlphaEq QuantifiedType where
    alphaEq' (Quantified quants1 t1) (Quantified quants2 t2) = (&&) <$> alphaEq' t1 t2 <*> alphaEq' quants1 quants2


instance AlphaEq HsModule where
    alphaEq' (HsModule _ _ _ _ ds1) (HsModule _ _ _ _ ds2) = alphaEqs' ds1 ds2
instance AlphaEq HsDecl where
    alphaEq' (HsPatBind _ pat1 rhs1 ds1) (HsPatBind _ pat2 rhs2 ds2) = do
        p <- alphaEq' pat1 pat2
        r <- alphaEq' rhs1 rhs2
        ds <- alphaEqs' ds1 ds2
        return $ p && r && ds
    alphaEq' _ _ = return False
instance AlphaEq HsPat where
    alphaEq' (HsPVar n1) (HsPVar n2) = alphaEq' (convertName n1 :: String) (convertName n2)
    alphaEq' (HsPLit l1) (HsPLit l2) = return $ l1 == l2
    alphaEq' (HsPApp con1 ps1) (HsPApp con2 ps2) = do
        c <- alphaEq' (convertName con1 :: String) (convertName con2) 
        ps <- alphaEqs' ps1 ps2
        return $ c && ps
    alphaEq' (HsPTuple ps1) (HsPTuple ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPList ps1) (HsPList ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPParen p1) (HsPParen p2) = alphaEq' p1 p2
    alphaEq' (HsPAsPat v1 p1) (HsPAsPat v2 p2) = do
        v <- alphaEq' (convertName v1 :: String) (convertName v2)
        p <- alphaEq' p1 p2
        return $ v && p
    alphaEq' HsPWildCard HsPWildCard = return True
    alphaEq' _ _ = return False
instance AlphaEq HsRhs where
    alphaEq' (HsUnGuardedRhs e1) (HsUnGuardedRhs e2) = alphaEq' e1 e2
    alphaEq' _ _ = return False
instance AlphaEq HsExp where
    alphaEq' (HsVar v1) (HsVar v2) = alphaEq' (convertName v1 :: String) (convertName v2)
    alphaEq' (HsCon c1) (HsCon c2) = alphaEq' (HsVar c1) (HsVar c2)
    alphaEq' (HsLit l1) (HsLit l2) = return $ l1 == l2
    alphaEq' (HsApp e1a e1b) (HsApp e2a e2b) = (&&) <$> alphaEq' e1a e2a <*> alphaEq' e1b e2b
    alphaEq' (HsNegApp e1) (HsNegApp e2) = alphaEq' e1 e2
    alphaEq' (HsLambda _ ps1 e1) (HsLambda _ ps2 e2) = (&&) <$> alphaEqs' ps1 ps2 <*> alphaEq' e1 e2
    alphaEq' (HsLet ds1 e1) (HsLet ds2 e2) = (&&) <$> alphaEqs' ds1 ds2 <*> alphaEq' e1 e2
    alphaEq' (HsIf e1a e1b e1c) (HsIf e2a e2b e2c) = and <$> zipWithM alphaEq' [e1a, e1b, e1c] [e2a, e2b, e2c]
    alphaEq' (HsTuple es1) (HsTuple es2) = alphaEqs' es1 es2
    alphaEq' (HsList es1) (HsList es2) = alphaEqs' es1 es2
    alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    -- Ignore parenthese completely
    --alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    --alphaEq' e1 (HsParen e2) e2 = alphaEq' e1 e2
    alphaEq' (HsExpTypeSig _ e1 t1) (HsExpTypeSig _ e2 t2) =do
        e <- alphaEq' e1 e2
        t <- alphaEq' (synToQualType t1) (synToQualType t2)
        return $ e && t
    alphaEq' _ _ = return False

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
stripExpParens (HsInfixApp e1 op e2) = HsInfixApp (stripExpParens e1) op (stripExpParens e2)
stripExpParens (HsApp e1 e2) = HsApp (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsNegApp e) = HsNegApp (stripExpParens e)
stripExpParens (HsLambda l ps e) = HsLambda l ps (stripExpParens e)
stripExpParens (HsIf c e1 e2) = HsIf (stripExpParens c) (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsTuple es) = HsTuple (map stripExpParens es)
stripExpParens (HsList es) = HsList (map stripExpParens es)
stripExpParens (HsExpTypeSig l e t) = HsExpTypeSig l (stripExpParens e) t
stripExpParens e = e