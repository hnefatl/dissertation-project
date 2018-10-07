{-# Language FlexibleContexts #-}

module Typechecker.Typeclasses where

import Prelude hiding (any)
import Data.Foldable (any)
import Data.List (intercalate, union)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Either
import Control.Monad.Except

import Typechecker.Types
import Typechecker.Substitution
import Typechecker.Unifier

-- A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeVar (TypeVariable "a" KindStar))`
data TypePredicate = IsInstance Id Type
    deriving (Eq)

instance Ord TypePredicate where
    compare (IsInstance id1 _) (IsInstance id2 _) = compare id1 id2

instance Show TypePredicate where
    show (IsInstance name t) = name ++ " " ++ assocShow True t

instance Substitutable TypePredicate where
    applySub sub (IsInstance name t) = IsInstance name (applySub sub t)
    getTypeVars (IsInstance _ t) = getTypeVars t

instance Unifiable TypePredicate where
    mgu (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = mgu t1 t2
        | otherwise = throwError "Class names are different"
    match (IsInstance name1 t1) (IsInstance name2 t2)
        | name1 == name2 = match t1 t2
        | otherwise = throwError "Class names are different"


-- A predicate-qualified thing. `Ord a => a` (`t` is a type), `Ord a => Ord [a]` (`t` is a predicate).
data Qualified t = Qual [TypePredicate] t
    deriving (Eq, Ord)

instance Show t => Show (Qualified t) where
    -- Handle eg. `Eq a => ...` vs `(Eq a, Show a) => ...`
    show (Qual [p] t) = show p ++ " => " ++ show t
    show (Qual ps t) = "(" ++ intercalate ", " (map show ps) ++ ") => " ++ show t

instance Substitutable t => Substitutable (Qualified t) where
    applySub sub (Qual ps t) = Qual (applySub sub ps) (applySub sub t)
    getTypeVars (Qual ps t) = getTypeVars ps `union` getTypeVars t


-- A typeclass is described as a set of superclasses and a set of instances
data TypeClass = Class (S.Set Id) (S.Set ClassInstance)
    deriving (Eq)
-- A typeclass instance is eg. `instance Ord a => Ord [a]`
type ClassInstance = Qualified TypePredicate

type ClassEnvironment = M.Map Id TypeClass

-- Get all superclasses of a given class
superclasses :: MonadError String m => Id -> ClassEnvironment -> m (S.Set Id)
superclasses name env = case M.lookup name env of
    Just (Class supers _) -> return supers
    Nothing -> throwError "No such class in the environment"

-- Get all instances of a given class
instances :: MonadError String m => Id -> ClassEnvironment -> m (S.Set ClassInstance)
instances name env = case M.lookup name env of
    Just (Class _ insts) -> return insts
    Nothing -> throwError "No such class in the environment"

emptyClassEnv :: ClassEnvironment
emptyClassEnv = M.empty

envContains :: ClassEnvironment -> Id -> Bool
envContains m k = isJust (M.lookup k m)

-- Add a typeclass with the given superclasses
-- Check that the class hasn't already been added and that all the superclasses exist
addClass :: MonadError String m => Id -> [Id] -> ClassEnvironment -> m ClassEnvironment
addClass name supers ce
    | ce `envContains` name = throwError "Class already exists"
    | not $ all (ce `envContains`) supers = throwError "Missing superclass"
    | otherwise = return $ M.insert name (Class (S.fromList supers) S.empty) ce

-- Add an instance of a superclass, with the given qualifiers.
-- Check that the superclass exists, and that there are no overlapping instances
addInstance :: MonadError String m => ClassInstance -> ClassEnvironment -> m ClassEnvironment
addInstance inst@(Qual _ (IsInstance classname _)) ce =
    case M.lookup classname ce of -- Find the class we're making an instance of
        Nothing -> throwError "Class doesn't exist"
        Just (Class supers otherInsts) -> do
            when $ any (inst `overlaps`) otherInsts $ throwError "Overlapping instances"
            return $ M.insert classname (Class supers (S.insert inst otherInsts)) ce
    where
        -- Two instances overlap if there's a substitution which unifies their heads
        overlaps (Qual _ head1) (Qual _ head2) = isRight (mgu head1 head2)
          