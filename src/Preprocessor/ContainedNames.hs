{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Utility functions for getting variable names from the parse tree
module Preprocessor.ContainedNames where

import           BasicPrelude
import           Control.Monad.Except    (MonadError, throwError)
import           Data.Foldable           (foldlM)
import qualified Data.Set                as S
import           Data.Text               (pack)
import           Language.Haskell.Syntax
import           TextShow                (TextShow, showt)

import           ExtraDefs               (synPrint)
import           Names


disjointUnion :: (MonadError Text m, Ord a, TextShow a) => S.Set a -> S.Set a -> m (S.Set a)
disjointUnion s1 s2 = if S.null inter then return (S.union s1 s2) else throwError err
    where inter = S.intersection s1 s2
          err = "Duplicate binding names in same level: " <> showt (S.toList inter)
disjointUnions :: (MonadError Text m, Foldable f, Ord a, TextShow a) => f (S.Set a) -> m (S.Set a)
disjointUnions = foldlM disjointUnion S.empty

class HasBoundVariables a where
    getBoundVariables :: MonadError Text m => a -> m (S.Set VariableName)
class HasFreeVariables a where
    getFreeVariables :: MonadError Text m => a -> m (S.Set VariableName)
class HasFreeTypeVariables a where
    getFreeTypeVariables :: a -> S.Set TypeVariableName

instance {-# Overlappable #-} (Traversable t, HasBoundVariables a) => HasBoundVariables (t a) where
    getBoundVariables = disjointUnions <=< mapM getBoundVariables
instance {-# Overlappable #-} (Traversable t, HasFreeVariables a) => HasFreeVariables (t a) where
    getFreeVariables = fmap S.unions . mapM getFreeVariables
instance {-# Overlappable #-} (Functor f, Foldable f, HasFreeTypeVariables a) => HasFreeTypeVariables (f a) where
    getFreeTypeVariables = S.unions . fmap getFreeTypeVariables

instance HasBoundVariables HsDecl where
    getBoundVariables (HsPatBind _ pat _ _) = getBoundVariables pat
    getBoundVariables (HsFunBind matches) = do
        let names = map (\(HsMatch _ name _ _ _) -> convertName name) matches
            funName = head names
            allNamesMatch = all (== funName) names
        if allNamesMatch then return $ S.singleton funName else throwError "Mismatched function names"
    getBoundVariables (HsTypeSig _ names _) = return $ S.fromList $ map convertName names
    getBoundVariables (HsClassDecl _ _ _ _ decls) = S.unions <$> mapM getBoundVariables decls
    getBoundVariables (HsDataDecl _ _ _ _ conDecls _) = getBoundVariables conDecls
    getBoundVariables d = throwError $ unlines ["Declaration not supported:", synPrint d]
instance HasBoundVariables HsConDecl where
    getBoundVariables (HsConDecl _ n _) = return $ S.singleton $ convertName n
    getBoundVariables (HsRecDecl _ n _) = return $ S.singleton $ convertName n
instance HasBoundVariables HsPat where
    getBoundVariables (HsPVar v)            = return $ S.singleton (convertName v)
    getBoundVariables (HsPLit _)            = return S.empty
    getBoundVariables HsPWildCard           = return S.empty
    getBoundVariables (HsPNeg p)            = getBoundVariables p
    getBoundVariables (HsPParen p)          = getBoundVariables p
    getBoundVariables (HsPIrrPat p)         = getBoundVariables p
    getBoundVariables (HsPAsPat v p)        = disjointUnion (S.singleton $ convertName v) =<< getBoundVariables p
    getBoundVariables (HsPInfixApp p1 _ p2) = getBoundVariables [p1, p2]
    getBoundVariables (HsPApp _ ps)         = getBoundVariables ps
    getBoundVariables (HsPTuple ps)         = getBoundVariables ps
    getBoundVariables (HsPList ps)          = getBoundVariables ps
    getBoundVariables (HsPRec _ _)          = throwError "Pattern records not supported"

instance HasFreeVariables HsDecl where
    getFreeVariables (HsPatBind _ _ rhs _)      = getFreeVariables rhs
    getFreeVariables (HsClassDecl _ _ _ _ args) = S.unions <$> mapM getFreeVariables args
    getFreeVariables HsTypeSig{}                = return S.empty
    getFreeVariables HsDataDecl{}               = return S.empty
    getFreeVariables _                          = throwError "Not supported"
instance HasFreeVariables HsRhs where
    getFreeVariables (HsUnGuardedRhs e) = getFreeVariables e
    getFreeVariables (HsGuardedRhss _)  = throwError "Guarded rhss not supported"
instance HasFreeVariables HsExp where
    getFreeVariables (HsVar name)          = return $ S.singleton $ convertName name
    getFreeVariables (HsCon _)             = return S.empty
    getFreeVariables (HsLit _)             = return S.empty
    getFreeVariables (HsInfixApp e1 op e2) = S.insert (convertName op) <$> getFreeVariables [e1, e2]
    getFreeVariables (HsApp e1 e2)         = S.union <$> getFreeVariables e1 <*> getFreeVariables e2
    getFreeVariables (HsNegApp e)          = getFreeVariables e
    getFreeVariables (HsLambda _ pats e)   = S.difference <$> getFreeVariables e <*> getBoundVariables pats
    getFreeVariables (HsLet ds e)          = S.union <$> getFreeVariables ds <*> getFreeVariables e
    getFreeVariables (HsIf e1 e2 e3)       = getFreeVariables [e1, e2, e3]
    getFreeVariables (HsTuple es)          = getFreeVariables es
    getFreeVariables (HsList es)           = getFreeVariables es
    getFreeVariables (HsParen e)           = getFreeVariables e
    getFreeVariables (HsExpTypeSig _ e _)  = getFreeVariables e
    getFreeVariables e                     = throwError $ pack $ "Unsupported expression " <> show e

instance HasFreeTypeVariables HsQualType where
    getFreeTypeVariables (HsQualType quals t) = S.union (getFreeTypeVariables quals) (getFreeTypeVariables t)
instance HasFreeTypeVariables HsType where
    getFreeTypeVariables (HsTyFun t1 t2) = S.union (getFreeTypeVariables t1) (getFreeTypeVariables t2)
    getFreeTypeVariables (HsTyTuple ts)  = getFreeTypeVariables ts
    getFreeTypeVariables (HsTyApp t1 t2) = S.union (getFreeTypeVariables t1) (getFreeTypeVariables t2)
    getFreeTypeVariables (HsTyVar n)     = S.singleton $ convertName n
    getFreeTypeVariables (HsTyCon _)     = S.empty
