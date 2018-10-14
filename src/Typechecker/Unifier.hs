{-# Language FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module Typechecker.Unifier where

import Control.Monad (join)
import Control.Monad.Except

import Typechecker.Substitution
import Typechecker.Types

class Unifiable t where
    -- Most general unifier of two types
    mgu :: MonadError String m => t -> t -> m Substitution
    -- Unification only applied to the first parameter (only type variables in $1 are unified)
    match :: MonadError String m => t -> t -> m Substitution

-- Only allow unification on instantiated types, those with globally unique type variable names.
-- Otherwise we can accidentally unify on a non-unique variable name, which is a big mistake
instance Unifiable InstantiatedType where
    mgu (TypeVar var) t2 = unifyVar var t2
    mgu t1 (TypeVar var) = unifyVar var t1
    mgu (TypeApp t1 t2) (TypeApp s1 s2) = subCompose <$> mgu t1 s1 <*> mgu t2 s2
    mgu (TypeConst tc1) (TypeConst tc2) 
        | tc1 == tc2 = return subEmpty
        | otherwise = throwError "Types don't unify: different constants"
    mgu _ _ = throwError "Types don't unify"

    match (TypeVar var) t2 = unifyVar var t2
    match (TypeApp t1 t2) (TypeApp s1 s2) = join $ subMerge <$> match t1 s1 <*> match t2 s2
    match (TypeConst tc1) (TypeConst tc2) 
        | tc1 == tc2 = return subEmpty
        | otherwise = throwError "Types don't unify: different constants"
    match _ _ = throwError "Types don't unify"
instance Unifiable t => Unifiable (TypePredicate t) where
    mgu (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = mgu t1 t2
        | otherwise = throwError "Class names are different"
    match (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = match t1 t2
        | otherwise = throwError "Class names are different"
instance Unifiable a => Unifiable (Qualified t a) where
    -- For Qualified things, we only unify on the head (the non-qualification bit), eg. in `Ord a => Ord [a]` we only
    -- unify on `Ord [a]`.
    mgu (Qualified _ x1) (Qualified _ x2) = mgu x1 x2
    match (Qualified _ x1) (Qualified _ x2) = match x1 x2


-- unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadError String m => TypeVariable -> InstantiatedType -> m Substitution
unifyVar var t
    | TypeVar var == t = return subEmpty
    | var `elem` getTypeVars t = throwError "Fails occurs check" -- The type contains the variable
    | otherwise = do
        varKind <- getKind var
        typeKind <- getKind t
        if varKind /= typeKind then throwError "Kind mismatch" else return (subSingle var t)