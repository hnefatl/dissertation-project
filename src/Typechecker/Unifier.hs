{-# Language FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module Typechecker.Unifier where

import Text.Printf
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
    mgu (TypeCon c1) (TypeCon c2)
        | c1 == c2 = return subEmpty
        | otherwise = throwError $ printf "Type constants don't unify: %s vs %s" (show c1) (show c2)
    mgu (TypeApp t1a t1b k1) (TypeApp t2a t2b k2)
        | k1 == k2 = mgu [t1a, t1b] [t2a, t2b]
        | otherwise = throwError $ printf "Mismatched kinds: %s vs %s" (show k1) (show k2)
    mgu t1 t2 = throwError $ printf "Failed to unify: %s vs %s" (show t1) (show t2)

    match (TypeVar var) t2 = unifyVar var t2
    match (TypeCon c1) (TypeCon c2)
        | c1 == c2 = return subEmpty
        | otherwise = throwError $ printf "Type constants don't match: %s vs %s" (show c1) (show c2)
    match (TypeApp t1a t1b k1) (TypeApp t2a t2b k2)
        | k1 == k2 = match [t1a, t1b] [t2a, t2b]
        | otherwise = throwError $ printf "Kinds don't match: %s vs %s" (show k1) (show k2)
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
    mgu xl yl = mgu' xl yl subEmpty
        where
            mgu' (x:xs) (y:ys) sub1 = do
                sub2 <- mgu (applySub sub1 x) (applySub sub1 y)
                mgu' xs ys (subCompose sub1 sub2)
            mgu' _ _ sub = return sub
    match xl yl = match' xl yl subEmpty
        where
            match' (x:xs) (y:ys) sub1 = do
                sub2 <- mgu (applySub sub1 x) (applySub sub1 y)
                match' xs ys =<< subMerge sub1 sub2
            match' _ _ sub = return sub
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