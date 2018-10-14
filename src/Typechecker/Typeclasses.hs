{-# Language FlexibleContexts, LambdaCase #-}

module Typechecker.Typeclasses where

import Prelude hiding (any)
import Data.Foldable
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Either
import Control.Monad.Except

import ExtraDefs
import Typechecker.Types
import Typechecker.Substitution
import Typechecker.Unifier

-- A typeclass instance is eg. `instance Ord a => Ord [a]` or `instance Ord Int`.
-- We restrict to globally unique type variable names (instantiated types)
type ClassInstance = Qualified InstantiatedType InstantiatedTypePredicate

-- A typeclass is described as a set of superclasses and a set of instances
-- A typeclass superclass is eg. `Eq` in `class Eq a => Ord a`
data TypeClass = Class (S.Set Id) (S.Set ClassInstance) deriving (Eq)

-- Qualified types need to match the same global unique names to the predicates as it does the head
type ClassEnvironment = M.Map Id TypeClass

-- Get all superclasses of a given class
superclasses :: MonadError String m => Id -> ClassEnvironment -> m (S.Set Id)
superclasses name env = case M.lookup name env of
    Just (Class supers _) -> return supers
    Nothing -> throwError ("No class " ++ show name ++ " in the environment")

-- Get all instances of a given class
instances :: MonadError String m => Id -> ClassEnvironment -> m (S.Set ClassInstance)
instances name env = case M.lookup name env of
    Just (Class _ insts) -> return insts
    Nothing -> throwError ("No class " ++ show name ++ " in the environment")

emptyClassEnv :: ClassEnvironment
emptyClassEnv = M.empty

-- Add a typeclass with the given superclasses
-- Check that the class hasn't already been added and that all the superclasses exist
addClass :: MonadError String m => Id -> S.Set Id -> ClassEnvironment -> m ClassEnvironment
addClass name supers ce
    | name `M.member` ce = throwError ("Class " ++ name ++ " already exists")
    | not $ null missingSupers = throwError ("Missing superclasses " ++ show missingSupers)
    | otherwise = return $ M.insert name (Class supers S.empty) ce
        where missingSupers = S.filter (not . (`M.member` ce)) supers

-- Add an instance of a superclass, with the given qualifiers.
-- Check that the superclass exists, and that there are no overlapping instances
addInstance :: MonadError String m => ClassInstance -> ClassEnvironment -> m ClassEnvironment
addInstance inst@(Qualified _ (IsInstance classname _)) ce =
    case M.lookup classname ce of -- Find the class we're making an instance of
        Nothing -> throwError ("Class " ++ classname ++ " doesn't exist")
        Just (Class supers otherInsts) -> do
            when (not $ null overlappingInstances) $ throwError ("Overlapping instances " ++ show overlappingInstances)
            return $ M.insert classname (Class supers (S.insert inst otherInsts)) ce
            where
                -- Two instances overlap if there's a substitution which unifies their heads
                overlaps (Qualified _ head1) (Qualified _ head2) = isRight (mgu head1 head2)
                overlappingInstances = S.filter (inst `overlaps`) otherInsts
          

-- If the given type predicate is true in the given class environment, then all the predicates returned from this
-- function are also true (obtained by considering all the superclasses)
ifPThenBySuper :: MonadError String m => ClassEnvironment -> InstantiatedTypePredicate -> m (S.Set InstantiatedTypePredicate)
ifPThenBySuper ce p@(IsInstance classname ty) = do
    supers <- S.toList <$> superclasses classname ce
    foldM mergeSupers (S.singleton p) supers
    where mergeSupers acc classname' = S.union acc <$> ifPThenBySuper ce (IsInstance classname' ty)

-- Same as above, but getting predicates by unifying with instances of the class.
-- The unification here is contained to this function: if a substitution is found, it's only used here and doesn't
-- escape, so we can use either instantiated or uninstantiated types.
ifPThenByInstance :: MonadError String m => ClassEnvironment -> InstantiatedTypePredicate -> m (Maybe (S.Set InstantiatedTypePredicate))
ifPThenByInstance ce p@(IsInstance classname _) = do
    insts <- instances classname ce
    -- Pick the first non-Nothing value (as we can't have overlapping instances, this is the right instance)
    return $ msum (S.map tryMatchInstance insts)
    where 
        tryMatchInstance (Qualified qualifiers hd) = do -- Maybe monad
            subs <- eitherToMaybe (match hd p) -- Find a substitution
            -- The new predicates are the constraints on the matching instance
            Just $ applySub subs qualifiers


-- Determines if the given predicate can be deduced from the given assumptions and the class environment
entails :: MonadError String m => ClassEnvironment -> S.Set InstantiatedTypePredicate -> InstantiatedTypePredicate -> m Bool
entails ce assumps p = (||) <$> entailedBySuperset <*> entailedByInstance
    where
        -- Can this predicate be satisifed by the superclasses?
        entailedBySuperset = (p `S.member`) . S.unions <$> mapM (ifPThenBySuper ce) (S.toList assumps)
        -- Can this predicate be satisfied by unification with other instances of this class?
        entailedByInstance = ifPThenByInstance ce p >>= \case
            Nothing -> return False
            Just qualifiers -> allM (entails ce assumps) qualifiers