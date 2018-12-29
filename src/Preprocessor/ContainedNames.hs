{-# LANGUAGE FlexibleContexts #-}

-- |Utility functions for getting variable names from the parse tree
module Preprocessor.ContainedNames where

import           BasicPrelude
import           Control.Monad.Except    (MonadError, throwError)
import           Data.Foldable           (foldlM)
import qualified Data.Set                as S
import           Data.Text               (pack)
import           Language.Haskell.Syntax
import           TextShow                (TextShow, showt)

import           Names


disjointUnion :: (MonadError Text m, Ord a, TextShow a) => S.Set a -> S.Set a -> m (S.Set a)
disjointUnion s1 s2 = if S.null inter then return (S.union s1 s2) else throwError err
    where inter = S.intersection s1 s2
          err = "Duplicate binding names in same level: " <> showt (S.toList inter)
disjointUnions :: (MonadError Text m, Foldable f, Ord a, TextShow a) => f (S.Set a) -> m (S.Set a)
disjointUnions = foldlM disjointUnion S.empty

-- |Get all variable names top-level bound by this declaration, **not recursively**. Eg. for the pattern binding:
-- > x = y
-- >   where y = 5
-- we return `x` but not `y`, as `x` is visible to horizontally defined bindings, whereas y is not.
-- We care about what variables are bound rather than contained for eg. typechecking mutually recursive statements.
getDeclBoundNames :: MonadError Text m => HsDecl -> m (S.Set VariableName)
getDeclBoundNames (HsPatBind _ pat _ _) = getPatContainedNames pat
getDeclBoundNames (HsFunBind matches) = do
    let names = map (\(HsMatch _ name _ _ _) -> convertName name) matches
        funName = head names
        allNamesMatch = all (== funName) names
    if allNamesMatch then return $ S.singleton funName else throwError "Mismatched function names"
getDeclBoundNames _ = throwError "Declaration not supported"
getDeclsBoundNames :: MonadError Text m => [HsDecl] -> m (S.Set VariableName)
getDeclsBoundNames ds = disjointUnions =<< mapM getDeclBoundNames ds


-- |Return all variable names contained in this pattern (recursively)
getPatContainedNames :: MonadError Text m => HsPat -> m (S.Set VariableName)
getPatContainedNames (HsPVar v) = return $ S.singleton (convertName v)
getPatContainedNames (HsPLit _) = return S.empty
getPatContainedNames HsPWildCard = return S.empty
getPatContainedNames (HsPNeg p) = getPatContainedNames p
getPatContainedNames (HsPParen p) = getPatContainedNames p
getPatContainedNames (HsPIrrPat p) = getPatContainedNames p
getPatContainedNames (HsPAsPat v p) = disjointUnion (S.singleton $ convertName v) =<< getPatContainedNames p
getPatContainedNames (HsPInfixApp p1 _ p2) = do
    ns1 <- getPatContainedNames p1
    ns2 <- getPatContainedNames p2
    disjointUnion ns1 ns2
getPatContainedNames (HsPApp _ ps) = disjointUnions =<< mapM getPatContainedNames ps
getPatContainedNames (HsPTuple ps) = disjointUnions =<< mapM getPatContainedNames ps
getPatContainedNames (HsPList ps) = disjointUnions =<< mapM getPatContainedNames ps
getPatContainedNames (HsPRec _ _) = throwError "Pattern records not supported"
-- |Get all variable names bound in this list of patterns
getPatsContainedNames :: MonadError Text m => [HsPat] -> m (S.Set VariableName)
getPatsContainedNames ps = disjointUnions =<< mapM getPatContainedNames ps


-- |Get all variable names contained within this declaration: excluding those names **bound** by the declaration.
getDeclContainedNames :: MonadError Text m => HsDecl -> m (S.Set VariableName)
getDeclContainedNames (HsPatBind _ _ rhs _) = getRhsContainedNames rhs
getDeclContainedNames (HsFunBind _)         = throwError "Variables in a HsMatch not supported"
getDeclContainedNames _                     = throwError "Not supported"
getDeclsContainedNames :: MonadError Text m => [HsDecl] -> m (S.Set VariableName)
getDeclsContainedNames ds = S.unions <$> mapM getDeclContainedNames ds

getRhsContainedNames :: MonadError Text m => HsRhs -> m (S.Set VariableName)
getRhsContainedNames (HsUnGuardedRhs e) = getExpContainedNames e
getRhsContainedNames (HsGuardedRhss _)  = throwError "Guarded rhss not supported"

getExpContainedNames :: MonadError Text m => HsExp -> m (S.Set VariableName)
getExpContainedNames (HsVar name) = return $ S.singleton $ convertName name
getExpContainedNames (HsCon _) = return S.empty
getExpContainedNames (HsLit _) = return S.empty
getExpContainedNames (HsInfixApp e1 op e2) = do
    let opNames = S.singleton $ convertName op
    e1Names <- getExpContainedNames e1
    e2Names <- getExpContainedNames e2
    return $ S.unions [opNames, e1Names, e2Names]
getExpContainedNames (HsApp e1 e2) = S.union <$> getExpContainedNames e1 <*> getExpContainedNames e2
getExpContainedNames (HsNegApp e) = getExpContainedNames e
getExpContainedNames (HsLambda _ pats e) = S.union <$> getPatsContainedNames pats <*> getExpContainedNames e
getExpContainedNames (HsLet ds e) = S.union <$> getDeclsContainedNames ds <*> getExpContainedNames e
getExpContainedNames (HsIf e1 e2 e3) = S.unions <$> mapM getExpContainedNames [e1, e2, e3]
getExpContainedNames (HsTuple es) = S.unions <$> mapM getExpContainedNames es
getExpContainedNames (HsList es) = S.unions <$> mapM getExpContainedNames es
getExpContainedNames (HsParen e) = getExpContainedNames e
getExpContainedNames (HsExpTypeSig _ e _) = getExpContainedNames e
getExpContainedNames e = throwError $ pack $ "Unsupported expression " <> show e

getQualTypeContainedNames :: HsQualType -> S.Set TypeVariableName
getQualTypeContainedNames (HsQualType quals t) = S.union (getAsstsContainedNames quals) (getTypeContainedNames t)
getAsstContainedNames :: HsAsst -> S.Set TypeVariableName
getAsstContainedNames (_, ts) = S.unions $ map getTypeContainedNames ts
getAsstsContainedNames :: [HsAsst] -> S.Set TypeVariableName
getAsstsContainedNames = S.unions . map getAsstContainedNames
getTypeContainedNames :: HsType -> S.Set TypeVariableName
getTypeContainedNames (HsTyFun t1 t2) = S.union (getTypeContainedNames t1) (getTypeContainedNames t2)
getTypeContainedNames (HsTyTuple ts)  = S.unions $ map getTypeContainedNames ts
getTypeContainedNames (HsTyApp t1 t2) = S.union (getTypeContainedNames t1) (getTypeContainedNames t2)
getTypeContainedNames (HsTyVar n)     = S.singleton $ convertName n
getTypeContainedNames (HsTyCon _)     = S.empty
