{-# Language FlexibleContexts #-}

module Preprocessor.ContainedNames where
-- |Utility functions for getting variable names from the parse tree

import Control.Monad.Except
import Language.Haskell.Syntax
import Data.Foldable
import Text.Printf
import qualified Data.Set as S

import ExtraDefs

disjointUnion :: (MonadError String m, Ord a, Show a) => S.Set a -> S.Set a -> m (S.Set a)
disjointUnion s1 s2 = if S.null inter then return (S.union s1 s2) else throwError err
    where inter = S.intersection s1 s2
          err = printf "Duplicate binding names in same level: %s" (show $ S.toList inter)
disjointUnions :: (MonadError String m, Foldable f, Ord a, Show a) => f (S.Set a) -> m (S.Set a)
disjointUnions = foldlM disjointUnion S.empty

-- |Get all variable names top-level bound by this declaration, **not recursively**. Eg. for the pattern binding:
-- > x = y
-- >   where y = 5
-- we return `x` but not `y`, as `x` is visible to horizontally defined bindings, whereas y is not.
-- We care about what variables are bound rather than contained for eg. typechecking mutually recursive statements.
getDeclBoundNames :: MonadError String m => HsDecl -> m (S.Set VariableName)
getDeclBoundNames (HsPatBind _ pat _ _) = getPatContainedNames pat
getDeclBoundNames (HsFunBind matches) = do
    let names = map (\(HsMatch _ name _ _ _) -> convertName name) matches
        funName = head names
        allNamesMatch = all (== funName) names
    if allNamesMatch then return $ S.singleton funName else throwError "Mismatched function names"
getDeclBoundNames _ = throwError "Declaration not supported"
getDeclsBoundNames :: MonadError String m => [HsDecl] -> m (S.Set VariableName)
getDeclsBoundNames ds = disjointUnions =<< mapM getDeclBoundNames ds


-- |Return all variable names contained in this pattern (recursively)
getPatContainedNames :: MonadError String m => HsPat -> m (S.Set VariableName)
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
getPatsContainedNames :: MonadError String m => [HsPat] -> m (S.Set VariableName)
getPatsContainedNames ps = disjointUnions =<< mapM getPatContainedNames ps


-- |Get all variable names contained within this declaration: excluding those names **bound** by the declaration.
getDeclContainedNames :: MonadError String m => HsDecl -> m (S.Set VariableName)
getDeclContainedNames (HsPatBind _ _ rhs _) = getRhsContainedNames rhs
getDeclContainedNames (HsFunBind _) = throwError "Variables in a HsMatch not supported"
getDeclContainedNames _ = throwError "Not supported"
getDeclsContainedNames :: MonadError String m => [HsDecl] -> m (S.Set VariableName)
getDeclsContainedNames ds = disjointUnions =<< mapM getDeclContainedNames ds

getRhsContainedNames :: MonadError String m => HsRhs -> m (S.Set VariableName)
getRhsContainedNames (HsUnGuardedRhs e) = getExpContainedNames e
getRhsContainedNames (HsGuardedRhss _) = throwError "Guarded rhss not supported"

getExpContainedNames :: MonadError String m => HsExp -> m (S.Set VariableName)
getExpContainedNames (HsVar name) = return $ S.singleton (convertName name)
getExpContainedNames (HsCon name) = return $ S.singleton (convertName name)
getExpContainedNames (HsLit _) = return S.empty
getExpContainedNames (HsInfixApp e1 op e2) = do
    let opNames = S.singleton (convertName op)
    e1Names <- getExpContainedNames e1
    e2Names <- getExpContainedNames e2
    disjointUnions [opNames, e1Names, e2Names]
getExpContainedNames (HsApp e1 e2) = do
    e1Names <- getExpContainedNames e1
    e2Names <- getExpContainedNames e2
    disjointUnion e1Names e2Names
getExpContainedNames (HsNegApp e) = getExpContainedNames e
getExpContainedNames (HsLambda _ pats e) = do
    patNames <- getPatsContainedNames pats
    eNames <- getExpContainedNames e
    disjointUnion patNames eNames
getExpContainedNames (HsLet ds e) = do
    dNames <- getDeclsContainedNames ds
    eNames <- getExpContainedNames e
    disjointUnion dNames eNames
getExpContainedNames (HsIf e1 e2 e3) = disjointUnions =<< mapM getExpContainedNames [e1, e2, e3]
getExpContainedNames (HsTuple es) = disjointUnions =<< mapM getExpContainedNames es
getExpContainedNames (HsList es) = disjointUnions =<< mapM getExpContainedNames es
getExpContainedNames (HsParen e) = getExpContainedNames e
getExpContainedNames e = throwError $ "Unsupported expression " ++ show e