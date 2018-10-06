module Typechecker.Substitution where

import Prelude hiding (all, fail)
import Control.Monad.Fail
import Data.List (union, nub)
import Data.Foldable (all)
import qualified Data.Map.Strict as M
import Typechecker.Types (TypeVariable(..), Type(..))

-- A substitution is a collection of assignments of a type to a variable
type Substitution = M.Map TypeVariable Type

subEmpty :: Substitution
subEmpty = M.empty

subSingle :: TypeVariable -> Type -> Substitution
subSingle = M.singleton

subMultiple :: [(TypeVariable, Type)] -> Substitution
subMultiple = M.fromList

class Substitutable t where
    -- Apply the given type variable -> type substitution
    applySub :: Substitution -> t -> t
    -- Return all the contained type variables, in left->right order and without duplicates
    getTypeVars :: t -> [TypeVariable]

instance Substitutable Type where
    applySub subs t@(TypeVar var) = M.findWithDefault t var subs
    applySub subs (TypeApp t1 t2) = TypeApp (applySub subs t1) (applySub subs t2)
    applySub _ t = t
    
    getTypeVars (TypeVar var) = [var]
    getTypeVars (TypeApp t1 t2) = getTypeVars t1 `union` getTypeVars t2
    getTypeVars _ = []
instance Substitutable a => Substitutable [a] where
    applySub subs = map (applySub subs)
    getTypeVars = nub . concatMap getTypeVars


-- Composition of substitutions
subCompose :: Substitution -> Substitution -> Substitution
subCompose s1 s2 = M.union s1 s2' -- Left-biased union with s1 applied to s2
    where s2' = M.map (applySub s1) s2

-- Merging of substitutions (the intersections of the type variables from each substitution must produce the same
-- results, the rest can do whatever).
subMerge :: MonadFail m => Substitution -> Substitution -> m Substitution
subMerge s1 s2 = if agree then return $ M.union s1 s2 else fail "Conflicting substitutions"
    where agree = all subsGiveSameResult (M.keys $ M.intersection s1 s2)
          -- Check that both substitutions give the same type when applied to the same type variables
          subsGiveSameResult var = applySub s1 (TypeVar var) == applySub s2 (TypeVar var)