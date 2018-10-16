{-# Language FlexibleContexts, FlexibleInstances, LambdaCase #-}

module Typechecker.Simplifier where

import Typechecker.Types
import Typechecker.Typeclasses

import Data.Foldable
import Control.Monad.Except
import qualified Data.Set as S

class HasHnf t where
    -- |Returns whether `t` is in head-normal form, as defined by the Haskell report
    -- In practice, this means that if we have environment `(Ord a, Ord b) => Ord (a, b)` and have `Ord (a, b)` in a
    -- qualifier we must expand it into `(Ord a, Ord b)`.
    inHnf :: t -> Bool

    -- |Converts a `t` into hnf
    toHnf :: (TypeInstantiator m, MonadError String m) => ClassEnvironment -> t -> m (S.Set UninstantiatedTypePredicate)

instance HasHnf (Type a) where
    inHnf (TypeVar _) = True
    inHnf (TypeConst _) = False
    inHnf (TypeApp t _) = inHnf t

    toHnf _ _ = throwError "Can't convert a type to HNF"

instance HasHnf UninstantiatedTypePredicate where
    inHnf (IsInstance _ x) = inHnf x

    -- |If the predicate is already in head normal form, return it. Otherwise, get the predicates that can be used to
    -- infer it from the environment.
    toHnf ce p | inHnf p = return (S.singleton p)
               | otherwise = ifPThenByInstance ce p >>= \case
                    Nothing -> throwError "Failed to convert predicate to HNF"
                    Just ps -> toHnf ce ps

instance HasHnf t => HasHnf (S.Set t) where
    inHnf = all inHnf
    toHnf ce s = S.unions <$> mapM (toHnf ce) (S.toList s)


-- |Removes redundant predicates from the given set. A predicate is redundant if it's entailed by any of the other
-- predicates
removeRedundant :: (TypeInstantiator m, MonadError String m) => ClassEnvironment -> S.Set UninstantiatedTypePredicate -> m (S.Set UninstantiatedTypePredicate)
removeRedundant ce s = foldlM removeIfEntailed S.empty s
    where removeIfEntailed acc p = do
            -- A predicate is redundant if it can be entailed by the other predicates
            redundant <- entails ce (acc `S.union` S.filter (> p) s) p
            return (if redundant then acc else S.insert p acc)

-- |Simplify a context as specified in the Haskell report: reduce each predicate to head-normal form then remove
-- redundant predicates.
simplify :: (TypeInstantiator m, MonadError String m) => ClassEnvironment -> S.Set UninstantiatedTypePredicate -> m (S.Set UninstantiatedTypePredicate)
simplify ce s = toHnf ce s >>= removeRedundant ce 