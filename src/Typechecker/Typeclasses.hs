{-# Language FlexibleContexts, LambdaCase #-}

module Typechecker.Typeclasses where

import Prelude hiding (any)
import Data.Foldable
import Data.Either
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad.Except

import ExtraDefs
import Typechecker.Types
import Typechecker.Substitution
import Typechecker.Unifier

type ClassName = Id

-- |A typeclass is described as a set of superclasses and a set of instances
-- A typeclass superclass is eg. `Eq` in `class Eq a => Ord a`
data TypeClass = Class (S.Set ClassName) (S.Set ClassInstance) deriving (Eq, Show)

-- |Qualified types need to match the same global unique names to the predicates as it does the head
type ClassEnvironment = M.Map Id TypeClass

-- |Get all superclasses of a given class
superclasses :: MonadError String m => Id -> ClassEnvironment -> m (S.Set Id)
superclasses name env = case M.lookup name env of
    Just (Class supers _) -> return supers
    Nothing -> throwError ("No class " ++ show name ++ " in the environment")

-- |Get all instances of a given class
instances :: MonadError String m => Id -> ClassEnvironment -> m (S.Set ClassInstance)
instances name env = case M.lookup name env of
    Just (Class _ insts) -> return insts
    Nothing -> throwError ("No class " ++ show name ++ " in the environment")

emptyClassEnv :: ClassEnvironment
emptyClassEnv = M.empty

-- |Add a typeclass with the given superclasses
-- Check that the class hasn't already been added and that all the superclasses exist
addClass :: MonadError String m => Id -> S.Set Id -> ClassEnvironment -> m ClassEnvironment
addClass name supers ce
    | name `M.member` ce = throwError ("Class " ++ name ++ " already exists")
    | not $ null missingSupers = throwError ("Missing superclasses " ++ show missingSupers)
    | otherwise = return $ M.insert name (Class supers S.empty) ce
        where missingSupers = S.filter (not . (`M.member` ce)) supers

-- |Add an instance of a superclass, with the given qualifiers.
-- Check that the superclass exists, and that there are no overlapping instances
addInstance :: MonadError String m => ClassInstance -> ClassEnvironment -> m ClassEnvironment
addInstance inst@(Qualified _ (IsInstance classname _)) ce =
    case M.lookup classname ce of -- Find the class we're making an instance of
        Nothing -> throwError ("Class " ++ classname ++ " doesn't exist")
        Just (Class supers otherInsts) -> do
            unless (null overlappingInstances) $ throwError ("Overlapping instances " ++ show overlappingInstances)
            return $ M.insert classname (Class supers (S.insert inst otherInsts)) ce
            where
                -- Two instances overlap if there's a substitution which unifies their heads
                overlaps (Qualified _ head1) (Qualified _ head2) = isRight (mgu head1 head2)
                overlappingInstances = S.filter (inst `overlaps`) otherInsts

-- |If the given type predicate is true in the given class environment, then all the predicates returned from this
-- function are also true (obtained by considering all the superclasses).
--
-- Given eg. `class Eq a => Ord a`, `ifPThenBySuper ce (IsInstance "Ord" t)` returns `{ IsInstance "Ord" t, IsInstance
-- "Eq" t }`
ifPThenBySuper :: MonadError String m => ClassEnvironment -> TypePredicate -> m (S.Set TypePredicate)
ifPThenBySuper ce p@(IsInstance classname ty) = do
    supers <- S.toList <$> superclasses classname ce
    foldM mergeSupers (S.singleton p) supers
    where mergeSupers acc classname' = S.union acc <$> ifPThenBySuper ce (IsInstance classname' ty)

-- |Same as above, but getting predicates by unifying with instances of the class - if we match the head of the
-- instance, return the qualifiers of the instance that we still need to show hold.
-- 
-- Given eg. `Ord a => Ord [a]`, `ifPThenByInstance ce (IsInstance "Ord" [(a,b)])` returns `IsInstance "Ord" (a,b)`.
ifPThenByInstance :: MonadError String m => ClassEnvironment -> TypePredicate -> m (Maybe (S.Set TypePredicate))
ifPThenByInstance ce p@(IsInstance classname _) = do
    insts <- instances classname ce
    -- See if any instances match the predicate we're exploring, and pick the first non-Nothing value (as we can't have
    -- overlapping instances, there's at most one instance)
    asum <$> mapM tryMatchInstance (S.toList insts)
    where tryMatchInstance (Qualified qualifiers t) = case match t p of -- Find a substitution
            Left _ -> return Nothing
            -- The new predicates are the constraints on the matching instance
            Right subs -> return $ Just $ applySub subs qualifiers


-- |Determines if the given predicate can be deduced from the given existing (assumed to be true) predicates and the
-- class environment
entails :: MonadError String m => ClassEnvironment -> S.Set TypePredicate -> TypePredicate -> m Bool
entails ce assumps p = (||) <$> entailedBySuperclass <*> entailedByInstance
    where
        -- Can this predicate be satisfied by the superclasses?
        entailedBySuperclass = (p `S.member`) . S.unions <$> mapM (ifPThenBySuper ce) (S.toList assumps)
        -- Can this predicate be satisfied by unification with other instances of this class?
        entailedByInstance = ifPThenByInstance ce p >>= \case
            Nothing -> return False
            Just qualifiers -> allM (entails ce assumps) qualifiers