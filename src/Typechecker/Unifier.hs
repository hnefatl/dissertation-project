{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Typechecker.Unifier where

import           BasicPrelude
import           Control.Monad.Except     (MonadError, throwError)
import           Data.Either              (isRight)
import qualified Data.Set                 as S
import           TextShow                 (TextShow, showt)
import           TextShow.Instances       ()

import           Typechecker.Substitution
import           Typechecker.Types

class (Ord t, TextShow t, Substitutable t) => Unifiable t where
    -- |Most general unifier of two types
    mgu :: MonadError Text m => t -> t -> m Substitution
    -- |Unification only applied to the first parameter (only type variables in $1 are unified)
    match :: MonadError Text m => t -> t -> m Substitution

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
        | otherwise = throwError $ "Type constants don't unify: " <> showt c1 <> " vs " <> showt c2
    mgu (TypeApp t1a t1b k1) (TypeApp t2a t2b k2)
        | k1 == k2 = mgu [t1a, t1b] [t2a, t2b]
        | otherwise = throwError $ "Mismatched kinds: " <> showt k1 <> " vs " <> showt k2
    mgu t1 t2 = throwError $ "Failed to unify: " <> showt t1 <> " vs " <> showt t2

    match (TypeVar var) t2 = unifyVar var t2
    match (TypeCon c1) (TypeCon c2)
        | c1 == c2 = return subEmpty
        | otherwise = throwError $ "Type constants don't match: " <> showt c1 <> " vs " <> showt c2
    match (TypeApp t1a t1b k1) (TypeApp t2a t2b k2)
        | k1 == k2 = match [t1a, t1b] [t2a, t2b]
        | otherwise = throwError $ "Kinds don't match: " <> showt k1 <> " vs " <> showt k2
    match t1 t2 = throwError $ "Failed to match: " <> showt t1 <> " vs " <> showt t2
instance Unifiable TypePredicate where
    mgu (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = mgu t1 t2
        | otherwise = throwError $ "Class names are different: " <> showt name1 <> " vs " <> showt name2
    match (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = match t1 t2
        | otherwise = throwError $ "Class names are different: " <> showt name1 <> " vs " <> showt name2
instance Unifiable t => Unifiable (Qualified t) where
    mgu (Qualified q1 x1) (Qualified q2 x2) = do
        s <- mgu x1 x2
        let (q1', q2') = (applySub s q1, applySub s q2)
        if q1' /= q2' then throwError $ "Qualifiers don't agree: " <> showt q1' <> " vs " <> showt q2' else return s
    match (Qualified q1 x1) (Qualified q2 x2) = do
        s <- match x1 x2
        let (q1', q2') = (applySub s q1, applySub s q2)
        if q1' /= q2' then throwError $ "Qualifiers don't agree: " <> showt q1' <> " vs " <> showt q2' else return s
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
    mgu _ _               = throwError "Mismatching Maybe types"

    match (Just x) (Just y) = match x y
    match _ _               = throwError "Mismatching Maybe types"


-- |unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadError Text m => TypeVariable -> Type -> m Substitution
unifyVar var@(TypeVariable name _) t
    | TypeVar var == t = return subEmpty
    | name `S.member` getTypeVars t = throwError $ "Fails occurs check: " <> showt var <> " vs " <> showt t
    | kind var /= kind t = throwError "Kind mismatch"
    | otherwise = return (subSingle name t)
