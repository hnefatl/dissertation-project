{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Typechecker.Substitution where

import           BasicPrelude
import           Control.Monad.Except (MonadError, throwError)
import           Data.Default         (Default, def)
import qualified Data.Map.Strict      as M
import qualified Data.Set             as S
import           Data.Text            (unpack)
import           TextShow             (TextShow, showb, showt)

import           Names
import           Typechecker.Types

-- |A substitution is a collection of assignments of a type to a variable
newtype Substitution a b = Substitution (M.Map a b) deriving (Eq)
type TypeSubstitution = Substitution TypeVariableName Type
type NameSubstitution = Substitution VariableName VariableName

instance Default (Substitution a b) where
    def = Substitution M.empty

instance (TextShow a, TextShow b) => TextShow (Substitution a b) where
    showb (Substitution subs) = "[" <> mconcat (intersperse ", " prettyElements) <> "]"
        where prettyElements = map (\(k, v) -> "(" <> showb v <> ")/" <> showb k) $ M.toList subs
instance (TextShow a, TextShow b) => Show (Substitution a b) where
    show = unpack . showt

class Substitutable a b t where
    -- |Apply the given type variable -> type substitution
    applySub :: Substitution a b -> t -> t
type TypeSubstitutable = Substitutable TypeVariableName Type

instance Ord a => Substitutable a a a where
    applySub (Substitution subs) v = M.findWithDefault v v subs

instance Substitutable TypeVariableName Type Type where
    applySub (Substitution subs) t@(TypeVar (TypeVariable name _)) = M.findWithDefault t name subs
    applySub (Substitution subs) t@(TypeCon (TypeConstant name _)) = M.findWithDefault t name subs
    applySub sub (TypeApp t1 t2 k)                                 = TypeApp (applySub sub t1) (applySub sub t2) k
instance Substitutable TypeVariableName Type TypePredicate where
    applySub sub (IsInstance name t) = IsInstance name (applySub sub t)
instance TypeSubstitutable a => Substitutable TypeVariableName Type (Qualified a) where
    applySub sub (Qualified ps x) = Qualified (applySub sub ps) (applySub sub x)
instance (Ord c, Substitutable a b c) => Substitutable a b (S.Set c) where
    applySub subs = S.map (applySub subs)
instance (Ord c, Substitutable a b d) => Substitutable a b (M.Map c d) where
    applySub subs = M.map (applySub subs)
instance Substitutable a b c => Substitutable a b [c] where
    applySub subs = map (applySub subs)
instance (Substitutable a b c, Substitutable a b d) => Substitutable a b (c, d) where
    applySub sub (a, b) = (applySub sub a, applySub sub b)
instance Substitutable a b d => Substitutable a b (Either c d) where
    applySub sub = fmap (applySub sub)
instance Substitutable a b c => Substitutable a b (Maybe c) where
    applySub sub = fmap (applySub sub)

subEmpty :: Substitution a b
subEmpty = def

subSingle :: Ord a => a -> b -> Substitution a b
subSingle v t = Substitution (M.singleton v t)

subMultiple :: (Ord a, Substitutable a b b) => [(a, b)] -> Substitution a b
subMultiple = subComposes . map (uncurry subSingle)


-- |Composition of substitutions
--
-- > (s1 `subCompose` s2) `applySub` <exp> = (s1 . s2)<exp> = s2(s1<exp>)
subCompose :: (Ord a, Substitutable a b b) => Substitution a b -> Substitution a b -> Substitution a b
subCompose (Substitution subs1) s2@(Substitution subs2) = Substitution (M.union subs1' subs2)
    where subs1' = M.map (applySub s2) subs1

subComposes :: (Ord a, Substitutable a b b) => [Substitution a b] -> Substitution a b
subComposes = foldl' subCompose subEmpty

-- |Merging of substitutions (the intersections of the type variables from each substitution must produce the same
-- results, the rest can do whatever).
subMerge :: (Ord a, Eq b, MonadError Text m, Substitutable a b b) => Substitution a b -> Substitution a b -> m (Substitution a b)
subMerge s1@(Substitution subs1) s2@(Substitution subs2) =
    if agree then return $ Substitution (M.union subs1 subs2) else throwError "Conflicting substitutions"
    where agree = all subsGiveSameResult (M.keys $ M.intersection subs1 subs2)
          -- Check that both substitutions give the same type when applied to the same type variables
          -- Ensures that eg. `[b/a, Int/b]` and `c/a, Int/c]` are merged
          subsGiveSameResult var = fmap (applySub s2) (M.lookup var subs1) == fmap (applySub s1) (M.lookup var subs2)

subMerges :: (Ord a, Eq b, MonadError Text m, Substitutable a b b) => [Substitution a b] -> m (Substitution a b)
subMerges = foldM subMerge subEmpty
