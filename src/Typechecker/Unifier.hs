{-# Language FlexibleContexts #-}

module Typechecker.Unifier where

import Control.Monad (when, liftM2, join)
import Control.Monad.Except

import Typechecker.Substitution
import Typechecker.Types

class Unifiable t where
    -- Most general unifier of two types
    mgu :: MonadError String m => t -> t -> m Substitution
    -- Unification only applied to the first parameter (only type variables in $1 are unified)
    match :: MonadError String m => t -> t -> m Substitution

instance Unifiable Type where
    mgu (TypeVar var) t2 = unifyVar var t2
    mgu t1 (TypeVar var) = unifyVar var t1
    mgu (TypeApp t1 t2) (TypeApp s1 s2) = subCompose <$> mgu t1 s1 <*> mgu t2 s2
    mgu (TypeConst tc1) (TypeConst tc2) 
        | tc1 == tc2 = return subEmpty
        | otherwise = throwError "Types don't unify: different constants"
    mgu _ _ = throwError "Types don't unify"

    match (TypeVar var) t2 = unifyVar var t2
    match (TypeApp t1 t2) (TypeApp s1 s2) = join $ liftM2 subMerge (match t1 s1) (match t2 s2)
    match (TypeConst tc1) (TypeConst tc2) 
        | tc1 == tc2 = return subEmpty
        | otherwise = throwError "Types don't unify: different constants"
    match _ _ = throwError "Types don't unify"


-- unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadError String m => TypeVariable -> Type -> m Substitution
unifyVar var t
    | TypeVar var == t = return subEmpty
    | var `elem` getTypeVars t = throwError "Fails occurs check" -- The type contains the variable
    | otherwise = do
        varKind <- getKind var
        typeKind <- getKind t
        when (varKind /= typeKind) (throwError "Kind mismatch")
        return $ subSingle var t