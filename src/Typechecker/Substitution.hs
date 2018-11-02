{-# Language FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}

module Typechecker.Substitution where

import Prelude hiding (all)
import Data.Default
import Data.Foldable
import Control.Monad.Except
import Data.List (intercalate)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Typechecker.Types

-- |A substitution is a collection of assignments of a type to a variable
newtype Substitution = Substitution (M.Map TypeVariableName Type) deriving (Eq)

instance Default Substitution where
    def = Substitution M.empty

instance Show Substitution where
    show (Substitution subs) = "[" ++ intercalate ", " prettyElements ++ "]"
        where prettyElements = map (\(k, v) -> "(" ++ show v ++ ")/" ++ k) $ M.toList subs

class Substitutable t where
    -- |Apply the given type variable -> type substitution
    applySub :: Substitution -> t -> t
    -- |Return all the contained type variables, in left->right order and without duplicates
    getTypeVars :: t -> S.Set TypeVariableName

-- |We only allow substitutions on instantiated variables: not on uninstantiated (dummy) ones
-- Building up substitutions on types with unintentionally overlapping variable names causes invalid unifications etc.
instance Substitutable Type where
    applySub (Substitution subs) t@(TypeVar (TypeVariable name _)) = M.findWithDefault t name subs
    applySub subs (TypeConstant name ks ts) = TypeConstant name ks (applySub subs ts)
    
    getTypeVars (TypeVar (TypeVariable name _)) = S.singleton name
    getTypeVars (TypeConstant _ _ ts) = getTypeVars ts
instance Substitutable TypePredicate where
    applySub sub (IsInstance name t) = IsInstance name (applySub sub t)
    getTypeVars (IsInstance _ t) = getTypeVars t
instance Substitutable a => Substitutable (Qualified a) where
    applySub sub (Qualified ps x) = Qualified (applySub sub ps) (applySub sub x)
    getTypeVars (Qualified ps x) = getTypeVars ps `S.union` getTypeVars x
instance (Ord t, Substitutable t) => Substitutable (S.Set t) where
    applySub subs = S.map (applySub subs)
    getTypeVars = getTypeVars . S.toList
instance (Ord t, Substitutable t) => Substitutable (M.Map a t) where
    applySub subs = M.map (applySub subs)
    getTypeVars = getTypeVars . M.elems
instance Substitutable t => Substitutable [t] where
    applySub subs = map (applySub subs)
    getTypeVars = S.unions . map getTypeVars
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    applySub sub (a, b) = (applySub sub a, applySub sub b)
    getTypeVars (a, b) = getTypeVars a `S.union` getTypeVars b
instance Substitutable b => Substitutable (Either a b) where
    applySub sub = fmap (applySub sub)
    getTypeVars = either (const S.empty) getTypeVars
instance Substitutable a => Substitutable (Maybe a) where
    applySub sub = fmap (applySub sub)
    getTypeVars = maybe S.empty getTypeVars

subEmpty :: Substitution
subEmpty = def

subSingle :: TypeVariableName -> Type -> Substitution
subSingle v t = Substitution (M.singleton v t)

subMultiple :: [(TypeVariableName, Type)] -> Substitution
subMultiple = foldl subCompose subEmpty . map (uncurry subSingle)


-- |Composition of substitutions
-- 
-- > (s1 `subCompose` s2) `applySub` <exp> = (s1 . s2)<exp> = s2(s1<exp>)
subCompose :: Substitution -> Substitution -> Substitution
subCompose (Substitution subs1) s2@(Substitution subs2) = Substitution (M.union subs1' subs2)
    where subs1' = M.map (applySub s2) subs1

-- |Merging of substitutions (the intersections of the type variables from each substitution must produce the same
-- results, the rest can do whatever).
subMerge :: MonadError String m => Substitution -> Substitution -> m Substitution
subMerge (Substitution subs1) (Substitution subs2) =
    if agree then return $ Substitution (M.union subs1 subs2) else throwError "Conflicting substitutions"
    where agree = all subsGiveSameResult (M.keys $ M.intersection subs1 subs2)
          -- Check that both substitutions give the same type when applied to the same type variables
          -- Ensures that eg. `[b/a, Int/b]` and `c/a, Int/c]` are merged
          subsGiveSameResult var = M.lookup var subs1 == M.lookup var subs2