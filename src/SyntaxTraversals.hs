{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module SyntaxTraversals where

import BasicPrelude
import Data.Foldable           (fold)
import Language.Haskell.Syntax

class SyntaxTraversable a where
    synTraverse :: Monad m => (HsDecl -> m HsDecl) -> (HsExp -> m HsExp) -> (HsPat -> m HsPat) -> a -> m a
    synMap :: (Monoid b, Monad m) => (HsDecl -> m b) -> (HsExp -> m b) -> (HsPat -> m b) -> a -> m b

declTraverse :: (SyntaxTraversable a, Monad m) => (HsDecl -> m HsDecl) -> a -> m a
declTraverse f = synTraverse f pure pure
declMap :: (SyntaxTraversable a, Monoid b, Monad m) => (HsDecl -> m b) -> a -> m b
declMap f = synMap f (const $ pure mempty) (const $ pure mempty)
expTraverse :: (SyntaxTraversable a, Monad m) => (HsExp -> m HsExp) -> a -> m a
expTraverse f = synTraverse pure f pure
expMap :: (SyntaxTraversable a, Monoid b, Monad m) => (HsExp -> m b) -> a -> m b
expMap f = synMap (const $ pure mempty) f (const $ pure mempty)
patTraverse :: (SyntaxTraversable a, Monad m) => (HsPat -> m HsPat) -> a -> m a
patTraverse = synTraverse pure pure
patMap :: (SyntaxTraversable a, Monoid b, Monad m) => (HsPat -> m b) -> a -> m b
patMap = synMap (const $ pure mempty) (const $ pure mempty)

instance (Traversable f, SyntaxTraversable a) => SyntaxTraversable (f a) where
    synTraverse fd fe fp = traverse (synTraverse fd fe fp)
    synMap fd fe fp = fmap fold . mapM (synMap fd fe fp)

instance SyntaxTraversable HsModule where
    synTraverse fd fe fp (HsModule a b c d decls) = HsModule a b c d <$> f decls
        where f = synTraverse fd fe fp
    synMap fd fe fp (HsModule _ _ _ _ decls) = synMap fd fe fp decls
instance SyntaxTraversable HsDecl where
    synTraverse fd fe fp x = fd x >>= \case
        HsPatBind loc pat rhs decls -> HsPatBind loc <$> f pat <*> f rhs <*> f decls
        HsFunBind matches           -> HsFunBind <$> f matches
        HsClassDecl loc ctx name args decls -> HsClassDecl loc ctx name args <$> f decls
        HsDataDecl loc ctx name args decls derives -> pure $ HsDataDecl loc ctx name args decls derives
        HsInstDecl loc ctx cname ts ds -> HsInstDecl loc ctx cname ts <$> f ds
        d -> pure d
        where f = synTraverse fd fe fp
    synMap fd fe fp x = ((<>) <$> fd x <*>) $ case x of
        HsPatBind _ pat rhs decls -> mconcat <$> sequence [f pat, f rhs, f decls]
        HsFunBind matches         -> f matches
        HsClassDecl _ _ _ _ decls -> f decls
        HsInstDecl _ _ _ _ decls  -> f decls
        _                         -> pure mempty
        where f = synMap fd fe fp
instance SyntaxTraversable HsRhs where
    synTraverse fd fe fp x = case x of
        HsUnGuardedRhs e   -> HsUnGuardedRhs <$> f e
        HsGuardedRhss rhss -> HsGuardedRhss <$> f rhss
        where f = synTraverse fd fe fp
    synMap fd fe fp x = case x of
        HsUnGuardedRhs e   -> f e
        HsGuardedRhss rhss -> f rhss
        where f = synMap fd fe fp
instance SyntaxTraversable HsGuardedRhs where
    synTraverse fd fe fp (HsGuardedRhs loc cond e) = HsGuardedRhs loc <$> f cond <*> f e
        where f = synTraverse fd fe fp
    synMap fd fe fp (HsGuardedRhs _ cond e) = f [cond, e]
        where f = synMap fd fe fp
instance SyntaxTraversable HsExp where
    synTraverse fd fe fp x = fe x >>= \case
        HsParen e           -> f e
        HsApp e1 e2         -> HsApp <$> f e1 <*> f e2
        HsInfixApp e1 op e2 -> HsInfixApp <$> f e1 <*> pure op <*> f e2
        HsNegApp e          -> HsNegApp <$> f e
        HsLambda l ps e     -> HsLambda l <$> f ps <*> f e
        HsIf c e1 e2        -> HsIf <$> f c <*> f e1 <*> f e2
        HsLet ds e          -> HsLet <$> f ds <*> f e
        HsTuple es          -> HsTuple <$> f es
        HsList es           -> HsList <$> f es
        HsExpTypeSig l e t  -> HsExpTypeSig l <$> f e <*> pure t
        e                   -> pure e
        where f = synTraverse fd fe fp
    synMap fd fe fp x = ((<>) <$> fe x <*>) $ case x of
        HsParen e          -> f e
        HsApp e1 e2        -> f [e1, e2]
        HsInfixApp e1 _ e2 -> f [e1, e2]
        HsNegApp e         -> f e
        HsLambda _ ps e    -> (<>) <$> f ps <*> f e
        HsIf c e1 e2       -> f [c, e1, e2]
        HsLet ds e         -> (<>) <$> f ds <*> f e
        HsTuple es         -> f es
        HsList es          -> f es
        HsExpTypeSig _ e _ -> f e
        _                  -> pure mempty
        where f = synMap fd fe fp
instance SyntaxTraversable HsPat where
    synTraverse fd fe fp x = fp x >>= \case
        HsPParen p           -> f p
        HsPNeg p             -> HsPNeg <$> f p
        HsPInfixApp p1 n p2  -> HsPInfixApp <$> f p1 <*> pure n <*> f p2
        HsPApp con ps        -> HsPApp con <$> f ps
        HsPTuple ps          -> HsPTuple <$> f ps
        HsPList ps           -> HsPList <$> f ps
        HsPAsPat n p         -> HsPAsPat n <$> f p
        HsPIrrPat p          -> HsPIrrPat <$> f p
        p                    -> pure p
        where f = synTraverse fd fe fp
    synMap fd fe fp x = ((<>) <$> fp x <*>) $ case x of
        HsPParen p          -> f p
        HsPNeg p            -> f p
        HsPInfixApp p1 _ p2 -> f [p1, p2]
        HsPApp _ ps         -> f ps
        HsPTuple ps         -> f ps
        HsPList ps          -> f ps
        HsPAsPat _ p        -> f p
        HsPIrrPat p         -> f p
        _                   -> pure mempty
        where f = synMap fd fe fp
instance SyntaxTraversable HsMatch where
    synTraverse fd fe fp (HsMatch loc name ps rhs wheres) = HsMatch loc name <$> f ps <*> f rhs <*> f wheres
        where f = synTraverse fd fe fp
    synMap fd fe fp (HsMatch _ _ ps rhs wheres) = mconcat <$> sequence [f ps, f rhs, f wheres]
        where f = synMap fd fe fp
