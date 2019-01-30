{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

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
newtype Substitution = Substitution (M.Map TypeVariableName Type) deriving (Eq)

instance Default Substitution where
    def = Substitution M.empty

instance TextShow Substitution where
    showb (Substitution subs) = "[" <> mconcat (intersperse ", " prettyElements) <> "]"
        where prettyElements = map (\(k, v) -> "(" <> showb v <> ")/" <> showb k) $ M.toList subs
instance Show Substitution where
    show = unpack . showt

class Substitutable t where
    -- |Apply the given type variable -> type substitution
    applySub :: Substitution -> t -> t

-- |We only allow substitutions on instantiated variables: not on uninstantiated (dummy) ones
-- Building up substitutions on types with unintentionally overlapping variable names causes invalid unifications etc.
instance Substitutable Type where
    applySub (Substitution subs) t@(TypeVar (TypeVariable name _)) = M.findWithDefault t name subs
    applySub (Substitution subs) t@(TypeCon (TypeConstant name _)) = M.findWithDefault t name subs
    applySub sub (TypeApp t1 t2 kind)                              = TypeApp (applySub sub t1) (applySub sub t2) kind
instance Substitutable TypePredicate where
    applySub sub (IsInstance name t) = IsInstance name (applySub sub t)
instance Substitutable a => Substitutable (Qualified a) where
    applySub sub (Qualified ps x) = Qualified (applySub sub ps) (applySub sub x)
instance (Ord t, Substitutable t) => Substitutable (S.Set t) where
    applySub subs = S.map (applySub subs)
instance (Ord t, Substitutable t) => Substitutable (M.Map a t) where
    applySub subs = M.map (applySub subs)
instance Substitutable t => Substitutable [t] where
    applySub subs = map (applySub subs)
instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
    applySub sub (a, b) = (applySub sub a, applySub sub b)
instance Substitutable b => Substitutable (Either a b) where
    applySub sub = fmap (applySub sub)
instance Substitutable a => Substitutable (Maybe a) where
    applySub sub = fmap (applySub sub)

subEmpty :: Substitution
subEmpty = def

subSingle :: TypeVariableName -> Type -> Substitution
subSingle v t = Substitution (M.singleton v t)

subMultiple :: [(TypeVariableName, Type)] -> Substitution
subMultiple = subComposes . map (uncurry subSingle)


-- |Composition of substitutions
--
-- > (s1 `subCompose` s2) `applySub` <exp> = (s1 . s2)<exp> = s2(s1<exp>)
subCompose :: Substitution -> Substitution -> Substitution
subCompose (Substitution subs1) s2@(Substitution subs2) = Substitution (M.union subs1' subs2)
    where subs1' = M.map (applySub s2) subs1

subComposes :: [Substitution] -> Substitution
subComposes = foldl' subCompose subEmpty

-- |Merging of substitutions (the intersections of the type variables from each substitution must produce the same
-- results, the rest can do whatever).
subMerge :: MonadError Text m => Substitution -> Substitution -> m Substitution
subMerge s1@(Substitution subs1) s2@(Substitution subs2) =
    if agree then return $ Substitution (M.union subs1 subs2) else throwError "Conflicting substitutions"
    where agree = all subsGiveSameResult (M.keys $ M.intersection subs1 subs2)
          -- Check that both substitutions give the same type when applied to the same type variables
          -- Ensures that eg. `[b/a, Int/b]` and `c/a, Int/c]` are merged
          subsGiveSameResult var = fmap (applySub s2) (M.lookup var subs1) == fmap (applySub s1) (M.lookup var subs2)

subMerges :: MonadError Text m => [Substitution] -> m Substitution
subMerges = foldM subMerge subEmpty
