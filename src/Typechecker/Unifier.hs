{-# Language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module Typechecker.Unifier where

import Text.Printf
import Data.Foldable
import Control.Monad
import Control.Monad.Except
import Data.Either
import qualified Data.Set as S

import Typechecker.Substitution
import Typechecker.Types

class (Ord t, Show t, Substitutable t) => Unifiable t where
    -- |Most general unifier of two types
    mgu :: MonadError String m => t -> t -> m Substitution
    -- |Unification only applied to the first parameter (only type variables in $1 are unified)
    match :: MonadError String m => t -> t -> m Substitution

    hasMgu :: t -> t -> Bool
    hasMgu x y = isRight (mgu x y)
    hasMatch :: t -> t -> Bool
    hasMatch x y = isRight (match x y)

-- |Only allow unification on instantiated types, those with globally unique type variable names.
-- Otherwise we can accidentally unify on a non-unique variable name, which is a big mistake
instance Unifiable Type where
    mgu (TypeVar var) t2 = unifyVar var t2
    mgu t1 (TypeVar var) = unifyVar var t1
    mgu (TypeConstant name1 ks1 ts1) (TypeConstant name2 ks2 ts2)
        | name1 /= name2 = throwError $ printf "Names don't unify: %s vs %s" (show name1) (show name2)
        | ks1 /= ks2 = throwError $ printf "Kinds don't unify: %s vs %s" (show ks1) (show ks2)
        | otherwise = mgu ts1 ts2

    match (TypeVar var) t2 = unifyVar var t2
    match (TypeConstant name1 ks1 ts1) (TypeConstant name2 ks2 ts2)
        | name1 /= name2 = throwError $ printf "Names don't match: %s vs %s" (show name1) (show name2)
        | ks1 /= ks2 = throwError $ printf "Kinds don't match: %s vs %s" (show ks1) (show ks2)
        | otherwise = match ts1 ts2
    match t1 t2 = throwError $ printf "Failed to match: %s vs %s" (show t1) (show t2)
instance Unifiable TypePredicate where
    mgu (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = mgu t1 t2
        | otherwise = throwError $ printf "Class names are different: %s vs %s" (show name1) (show name2)
    match (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = match t1 t2
        | otherwise = throwError $ printf "Class names are different: %s vs %s" (show name1) (show name2)
instance Unifiable t => Unifiable (Qualified t) where
    mgu (Qualified q1 x1) (Qualified q2 x2) = do
        s <- mgu x1 x2
        let (q1', q2') = (applySub s q1, applySub s q2)
        if q1' /= q2' then throwError $ printf "Qualifiers don't agree: %s vs %s" (show q1') (show q2') else return s
    match (Qualified q1 x1) (Qualified q2 x2) = do
        s <- match x1 x2
        let (q1', q2') = (applySub s q1, applySub s q2)
        if q1' /= q2' then throwError $ printf "Qualifiers don't agree: %s vs %s" (show q1') (show q2') else return s
instance Unifiable t => Unifiable [t] where
    mgu xs ys = foldl' subCompose subEmpty <$> zipWithM mgu xs ys
    match xs ys = foldlM subMerge subEmpty =<< zipWithM match xs ys
instance Unifiable t => Unifiable (Maybe t) where
    mgu (Just x) (Just y) = mgu x y
    mgu _ _ = throwError "Mismatching Maybe types"

    match (Just x) (Just y) = match x y
    match _ _ = throwError "Mismatching Maybe types"


-- |unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadError String m => TypeVariable -> Type -> m Substitution
unifyVar var@(TypeVariable name _) t
    | TypeVar var == t = return subEmpty
    | name `S.member` getTypeVars t = throwError $ printf "Fails occurs check: %s with %s" (show var) (show t)
    | getKind var /= getKind t = throwError "Kind mismatch"
    | otherwise = return (subSingle name t)