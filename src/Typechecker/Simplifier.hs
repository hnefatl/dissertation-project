{-# Language FlexibleContexts, LambdaCase #-}

module Typechecker.Simplifier where

import Typechecker.Types
import Typechecker.Typeclasses

import Data.Foldable
import Control.Monad.Except
import qualified Data.Set as S

class HasHnf t where
    inHnf :: t -> Bool
    toHnf :: MonadError String m => ClassEnvironment -> t -> m (S.Set TypePredicate)

instance HasHnf (Type a) where
    -- Returns whether the type is in head-normal form, as defined by the Haskell report
    -- In HNF iff it has the form `v t1 ... tn` where `v` is a type variable and `ti` are types, n >= 0
    inHnf (TypeVar _) = True
    inHnf (TypeConst _) = False
    inHnf (TypeApp t _) = inHnf t

    toHnf _ _ = throwError "Can't convert a type to HNF"

instance HasHnf TypePredicate where
    inHnf (IsInstance _ ty) = inHnf ty

    toHnf ce p | inHnf p = return (S.singleton p)
               | otherwise = ifPThenByInstance ce p >>= \case
                    Nothing -> throwError "Failed to convert predicate to HNF"
                    Just ps -> toHnf ce ps

instance HasHnf t => HasHnf (S.Set t) where
    inHnf = all inHnf
    toHnf ce s = S.unions <$> mapM (toHnf ce) (S.toList s)


removeRedundant :: MonadError String m => ClassEnvironment -> S.Set TypePredicate -> m (S.Set TypePredicate)
removeRedundant ce s = foldlM removeIfEntailed S.empty s
    where removeIfEntailed acc p = do
            -- A predicate is redundant if it can be entailed by the other predicates
            redundant <- entails ce (acc `S.union` (S.filter (> p) s)) p
            return (if redundant then acc else S.insert p acc)

-- Simplify a context as specified in the Haskell report: reduce each predicate to head-normal form then remove
-- redundant predicates.
simplify :: MonadError String m => ClassEnvironment -> S.Set TypePredicate -> m (S.Set TypePredicate)
simplify ce s = toHnf ce s >>= removeRedundant ce 