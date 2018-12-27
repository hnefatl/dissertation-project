{-# Language GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import BasicPrelude
import TextShow (TextShow, showt)
import TextShow.Instances ()
import Control.Monad.State.Strict (MonadState, State, runState, evalState, modify, gets)
import Control.Monad.Except (MonadError, ExceptT, runExceptT, throwError, catchError, liftEither)
import Control.Monad.Extra (findM)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Haskell.Syntax
import Data.Either (isRight)

import ExtraDefs (prettyPrintT)
import Names
import TextShowHsSrc ()
import Typechecker.Types
import Backend.ILA

newtype AlphaEqM a = AlphaEqM { inner :: ExceptT Text (State (M.Map Text Text)) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map Text Text), MonadError Text)

class TextShow a => AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM ()
alphaEqBool' :: AlphaEq a => a -> a -> AlphaEqM Bool
alphaEqBool' x y = catchError (alphaEq' x y >> return True) (const $ return False)

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = isRight $ alphaEqError x y
alphaEqError :: (MonadError Text m, AlphaEq a) => a -> a -> m ()
alphaEqError x y = liftEither $ evalState (runExceptT $ inner $ alphaEq' x y) M.empty

runAlphaEq :: AlphaEq a => a -> a -> (Maybe Text, M.Map Text Text)
runAlphaEq x y = (either Just (const Nothing) result, s)
    where (result, s) = runState (runExceptT $ inner $ alphaEq' x y) M.empty

-- The workhorse: drives renaming names
instance AlphaEq Text where
    alphaEq' s1 s2
        | s1 == s2 = return ()
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) ->
                unless (x == s2 && y == s1) $ throwError $ s1 <> " and " <> s2 <> " have already been renamed"
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (Nothing, _) -> throwError $ s1 <> " has been renamed but " <> s2 <> " hasn't"
            (_, Nothing) -> throwError $ s2 <> " has been renamed but " <> s1 <> " hasn't"


-- Standard useful instances
-- Lists compare pairwise
instance AlphaEq a => AlphaEq [a] where
    alphaEq' [] [] = return ()
    alphaEq' (x:xs) (y:ys) = alphaEq' x y >> alphaEq' xs ys
    alphaEq' xs ys = throwError $ unlines ["List length mismatch:", showt xs, "vs", showt ys]
-- Pairs compare... pairwise
instance (AlphaEq a, AlphaEq b) => AlphaEq (a, b) where
    alphaEq' (x1, y1) (x2, y2) = alphaEq' x1 x2 >> alphaEq' y1 y2
-- Sets compare in any order
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return () -- Both empty, both alpha equivalent
        | S.null s1 = throwError $ "Set is non-empty: %s" <> showt s2
        | S.null s2 = throwError $ "Set is non-empty: %s" <> showt s1
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            findM (alphaEqBool' x) (S.toList s2) >>= \case -- Find an alpha-eq element from the other set
                Nothing -> throwError $ "Couldn't find alpha-eq element for " <> showt x <> " in " <> showt s2
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse
-- Maps compare in any order by converting to sets
instance (Ord a, Ord b, AlphaEq a, AlphaEq b) => AlphaEq (M.Map a b) where
    alphaEq' m1 m2 = alphaEq' (S.fromList $ M.toList m1) (S.fromList $ M.toList m2)


-- Typechecker instances
instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2
instance AlphaEq VariableName where
    alphaEq' (VariableName s1) (VariableName s2) = alphaEq' s1 s2
instance AlphaEq TypeVariable where
    alphaEq' (TypeVariable n1 k1) (TypeVariable n2 k2) = do
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt n1 <> " vs " <> showt n2
        alphaEq' n1 n2
instance AlphaEq TypeConstant where
    alphaEq' (TypeConstant n1 k1) (TypeConstant n2 k2) = do
        unless (n1 == n2) $ throwError $ "Name mismatch: " <> showt n1 <> " vs " <> showt n2
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt n1 <> " vs " <> showt n2
instance AlphaEq Type where
    alphaEq' (TypeVar t1) (TypeVar t2) = alphaEq' t1 t2
    alphaEq' (TypeCon c1) (TypeCon c2) = alphaEq' c1 c2
    alphaEq' (TypeApp t1a t1b k1) (TypeApp t2a t2b k2) = do
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt k1 <> " vs " <> showt k2
        alphaEq' t1a t2a
        alphaEq' t1b t2b
    alphaEq' t1 t2 = throwError $ "Different types: " <> showt t1 <> " vs " <> showt t2
instance AlphaEq TypePredicate where
    alphaEq' (IsInstance c1 t1) (IsInstance c2 t2) = do
        unless (c1 == c2) $ throwError $ "Class mismatch: " <> showt c1 <> " vs " <> showt t1
        alphaEq' t1 t2
instance AlphaEq a => AlphaEq (Qualified a) where
    alphaEq' (Qualified quals1 t1) (Qualified quals2 t2) = alphaEq' t1 t2 >> alphaEq' quals1 quals2
instance AlphaEq QuantifiedType where
    alphaEq' (Quantified quants1 t1) (Quantified quants2 t2) = alphaEq' t1 t2 >> alphaEq' quants1 quants2


-- haskell-src instances
instance AlphaEq HsName where
    alphaEq' n1 n2 = alphaEq' (convertName n1 :: Text) (convertName n2)
instance AlphaEq HsQName where
    alphaEq' n1 n2 = alphaEq' (convertName n1 :: Text) (convertName n2)
instance AlphaEq HsModule where
    alphaEq' (HsModule _ _ _ _ ds1) (HsModule _ _ _ _ ds2) = alphaEq' ds1 ds2
instance AlphaEq HsDecl where
    alphaEq' (HsPatBind _ pat1 rhs1 ds1) (HsPatBind _ pat2 rhs2 ds2) = do
        alphaEq' pat1 pat2
        alphaEq' rhs1 rhs2
        alphaEq' ds1 ds2
    alphaEq' d1 d2 = throwError $ unlines [ "Different declaration types:", showt d1, "vs", showt d2 ]
instance AlphaEq HsPat where
    alphaEq' (HsPVar n1) (HsPVar n2) = alphaEq' (convertName n1 :: Text) (convertName n2)
    alphaEq' (HsPLit l1) (HsPLit l2) =
        unless (l1 == l2) $ throwError $ unlines [ "Literal pat mismatch:", prettyPrintT l1, "vs", prettyPrintT l2 ]
    alphaEq' (HsPApp con1 ps1) (HsPApp con2 ps2) = do
        alphaEq' (convertName con1 :: Text) (convertName con2) 
        alphaEq' ps1 ps2
    alphaEq' (HsPTuple ps1) (HsPTuple ps2) = alphaEq' ps1 ps2
    alphaEq' (HsPList ps1) (HsPList ps2) = alphaEq' ps1 ps2
    alphaEq' (HsPParen p1) (HsPParen p2) = alphaEq' p1 p2
    alphaEq' (HsPAsPat v1 p1) (HsPAsPat v2 p2) = do
        alphaEq' (convertName v1 :: Text) (convertName v2)
        alphaEq' p1 p2
    alphaEq' HsPWildCard HsPWildCard = return ()
    alphaEq' p1 p2 = throwError $ unlines [ "Pattern mismatch:", prettyPrintT p1, "vs", prettyPrintT p2 ]
instance AlphaEq HsRhs where
    alphaEq' (HsUnGuardedRhs e1) (HsUnGuardedRhs e2) = alphaEq' e1 e2
    alphaEq' r1 r2 = throwError $ unlines [ "RHS mismatch:", prettyPrintT r1, "vs", prettyPrintT r2 ]
instance AlphaEq HsExp where
    alphaEq' (HsVar v1) (HsVar v2) = alphaEq' (convertName v1 :: Text) (convertName v2)
    alphaEq' (HsCon c1) (HsCon c2) = alphaEq' (HsVar c1) (HsVar c2)
    alphaEq' (HsLit l1) (HsLit l2) =
        unless (l1 == l2) $ throwError $ "Literal exp mismatch: " <> prettyPrintT l1 <> " " <> prettyPrintT l2
    alphaEq' (HsApp e1a e1b) (HsApp e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (HsNegApp e1) (HsNegApp e2) = alphaEq' e1 e2
    alphaEq' (HsLambda _ ps1 e1) (HsLambda _ ps2 e2) = alphaEq' ps1 ps2 >> alphaEq' e1 e2
    alphaEq' (HsLet ds1 e1) (HsLet ds2 e2) = alphaEq' ds1 ds2 >> alphaEq' e1 e2
    alphaEq' (HsIf e1a e1b e1c) (HsIf e2a e2b e2c) = alphaEq' [e1a, e1b, e1c] [e2a, e2b, e2c]
    alphaEq' (HsTuple es1) (HsTuple es2) = alphaEq' es1 es2
    alphaEq' (HsList es1) (HsList es2) = alphaEq' es1 es2
    alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    alphaEq' (HsExpTypeSig _ e1 t1) (HsExpTypeSig _ e2 t2) = alphaEq' e1 e2 >> alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "Expression mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq HsType where
    alphaEq' (HsTyFun t1a t1b) (HsTyFun t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyTuple ts1) (HsTyTuple ts2) = alphaEq' ts1 ts2
    alphaEq' (HsTyApp t1a t1b) (HsTyApp t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyVar v1) (HsTyVar v2) = alphaEq' (convertName v1 :: Text) (convertName v2)
    alphaEq' (HsTyCon v1) (HsTyCon v2) =
        unless (v1 == v2) $ throwError $ "Name mismatch: " <> prettyPrintT v1 <> " vs " <> prettyPrintT v2
    alphaEq' t1 t2 = throwError $ "Type mismatch: " <> prettyPrintT t1 <> " vs " <> prettyPrintT t2
instance AlphaEq HsQualType where
    alphaEq' (HsQualType c1 t1) (HsQualType c2 t2) = alphaEq' c1 c2 >> alphaEq' t1 t2


-- ILA instances
instance AlphaEq Literal where
    alphaEq' (LiteralInt i1) (LiteralInt i2) =
        unless (i1 == i2) $ throwError $ "Integer literal mismatch:" <> showt i1 <> " vs " <> showt i2
    alphaEq' (LiteralFrac f1) (LiteralFrac f2) =
        unless (f1 == f2) $ throwError $ "Rational literal mismatch:" <> showt f1 <> " vs " <> showt f2
    alphaEq' (LiteralChar c1) (LiteralChar c2) =
        unless (c1 == c2) $ throwError $ "Character literal mismatch:" <> showt c1 <> " vs " <> showt c2
    alphaEq' (LiteralString s1) (LiteralString s2) =
        unless (s1 == s2) $ throwError $ "Text literal mismatch:" <> showt s1 <> " vs " <> showt s2
    alphaEq' l1 l2 = throwError $ "Literal mismatch:" <> showt l1 <> " vs " <> showt l2
instance AlphaEq Alt where
    alphaEq' (Alt ac1 vs1 e1) (Alt ac2 vs2 e2) = alphaEq' ac1 ac2 >> alphaEq' vs1 vs2 >> alphaEq' e1 e2
instance AlphaEq AltConstructor where
    alphaEq' (DataCon v1) (DataCon v2) = alphaEq' v1 v2
    alphaEq' (LitCon l1) (LitCon l2) = alphaEq' l1 l2
    alphaEq' Default Default = return ()
    alphaEq' c1 c2 = throwError $ unlines [ "Alt constructor mismatch:", showt c1, "vs", showt c2 ]
instance AlphaEq Expr where
    alphaEq' (Var n1) (Var n2) = alphaEq' n1 n2
    alphaEq' (Lit l1) (Lit l2) = alphaEq' l1 l2
    alphaEq' (App e1a e1b) (App e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (Lam v1 e1) (Lam v2 e2) = alphaEq' v1 v2 >> alphaEq' e1 e2
    alphaEq' (Let v1 e1a e1b) (Let v2 e2a e2b) = alphaEq' v1 v2 >> alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (Case e1 vs1 as1) (Case e2 vs2 as2) = alphaEq' e1 e2 >> alphaEq' vs1 vs2 >> alphaEq' as1 as2
    alphaEq' (Type t1) (Type t2) = alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "Expression mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq Binding where
    alphaEq' (NonRec v1 e1) (NonRec v2 e2) = alphaEq' v1 v2 >> alphaEq' e1 e2
    alphaEq' (Rec m1) (Rec m2) = alphaEq' m1 m2
    alphaEq' b1 b2 = throwError $ unlines [ "Binding mismatch:", showt b1, "vs", showt b2 ]


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