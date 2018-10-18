{-# Language FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
{-# Language ScopedTypeVariables, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Typechecker.Unifier where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Either

import Typechecker.Substitution
import Typechecker.Types

class Unifiable t where
    -- |Most general unifier of two types
    mgu :: MonadError String m => t -> t -> m Substitution
    -- |Unification only applied to the first parameter (only type variables in $1 are unified)
    match :: MonadError String m => t -> t -> m Substitution

-- |Only allow unification on instantiated types, those with globally unique type variable names.
-- Otherwise we can accidentally unify on a non-unique variable name, which is a big mistake
instance Unifiable InstantiatedType where
    mgu (TypeVar var) t2 = unifyVar var t2
    mgu t1 (TypeVar var) = unifyVar var t1
    mgu (TypeApp l1 l2) (TypeApp r1 r2) = do
        s1 <- mgu l1 r1
        s2 <- mgu (applySub s1 l2) (applySub s1 r2)
        return (subCompose s1 s2)
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
    -- |For Qualified things, we only unify on the head (the non-qualification bit), eg. in `Ord a => Ord [a]` we only
    -- unify on `Ord [a]`. TODO(kc506): find reference in the haskell report
    mgu (Qualified _ x1) (Qualified _ x2) = mgu x1 x2
    match (Qualified _ x1) (Qualified _ x2) = match x1 x2
instance Unifiable a => Unifiable (Maybe a) where
    mgu (Just x) (Just y) = mgu x y
    mgu _ _ = throwError "Mismatching Maybe types"

    match (Just x) (Just y) = match x y
    match _ _ = throwError "Mismatching Maybe types"


-- |unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadError String m => TypeVariable -> InstantiatedType -> m Substitution
unifyVar var t
    | TypeVar var == t = return subEmpty
    | var `elem` getTypeVars t = throwError "Fails occurs check" -- The type contains the variable
    | otherwise = do
        varKind <- getKind var
        typeKind <- getKind t
        if varKind /= typeKind then throwError "Kind mismatch" else return (subSingle var t)



-- Utilities for testing for alpha equivalence - one-off unification where the substitution doesn't escape, so we can
-- use variable names that are unique only to this call.

newtype IsolatedTypeInstantiator a = ITI (State Int a)
    deriving (Functor, Applicative, Monad, MonadState Int)
instance TypeInstantiator IsolatedTypeInstantiator where
    freshName = state (\s -> ("v" ++ show s, s + 1))

alphaEq :: (Instantiable a b, Unifiable b) => a -> a -> Bool
alphaEq t1 t2 = isRight (runExcept $ mgu t1' t2')
    where ITI s = (,) <$> doInstantiate t1 <*> doInstantiate t2
          (t1', t2') = evalState s 0