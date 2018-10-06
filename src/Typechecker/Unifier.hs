module Typechecker.Unifier where

import Prelude hiding (fail)
import Control.Monad (when, liftM2, join)
import Control.Monad.Fail

import Typechecker.Substitution
import Typechecker.Types

-- Most general unifier of two types
mgu :: MonadFail m => Type -> Type -> m Substitution
mgu (TypeVar var) t2 = unifyVar var t2
mgu t1 (TypeVar var) = unifyVar var t1
mgu (TypeApp t1 t2) (TypeApp s1 s2) = subCompose <$> mgu t1 s1 <*> mgu t2 s2
mgu (TypeConst tc1) (TypeConst tc2) 
    | tc1 == tc2 = return subEmpty
    | otherwise = fail "Types don't unify: different constants"
mgu _ _ = fail "Types don't unify"


-- Unification only applied to the first parameter (only type variables in $1 are unified)
match :: MonadFail m => Type -> Type -> m Substitution
match (TypeVar var) t2 = unifyVar var t2
match (TypeApp t1 t2) (TypeApp s1 s2) = join $ liftM2 subMerge (match t1 s1) (match t2 s2)
match (TypeConst tc1) (TypeConst tc2) 
    | tc1 == tc2 = return subEmpty
    | otherwise = fail "Types don't unify: different constants"
match _ _ = fail "Types don't unify"


-- unifyVar v t returns a substitution [t/v] like subSingle but performs additional checks
unifyVar :: MonadFail m => TypeVariable -> Type -> m Substitution
unifyVar var t
    | TypeVar var == t = return subEmpty
    | var `elem` getTypeVars t = fail "Fails occurs check" -- The type contains the variable
    | otherwise = do
        varKind <- getKind var
        typeKind <- getKind t
        when (varKind /= typeKind) (fail "Kind mismatch")
        return $ subSingle var t