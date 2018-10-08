{-# Language FlexibleContexts, LambdaCase #-}

module Typechecker.Typeclasses where

import Prelude hiding (any)
import Data.Foldable
import Data.List (intercalate, union)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Either
import Control.Monad.Except

import ExtraDefs
import Typechecker.Types
import Typechecker.Substitution
import Typechecker.Unifier

-- A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeVar (TypeVariable "a" KindStar))`
data TypePredicate = IsInstance Id ConcreteType
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
data Qualified t = Qual (S.Set TypePredicate) t
    deriving (Eq, Ord)

instance Show t => Show (Qualified t) where
    -- Handle eg. `Eq a => ...` vs `(Eq a, Show a) => ...`
    show (Qual quals t) = pquals ++ " => " ++ show t
        where 
        qualifiers = intercalate ", " (map show $ S.toList quals)
        pquals = if S.size quals > 1 then "(" ++ qualifiers ++ ")" else qualifiers

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

-- Add a typeclass with the given superclasses
-- Check that the class hasn't already been added and that all the superclasses exist
addClass :: MonadError String m => Id -> [Id] -> ClassEnvironment -> m ClassEnvironment
addClass name supers ce
    | name `M.member` ce = throwError "Class already exists"
    | not $ all (`M.member` ce) supers = throwError "Missing superclass"
    | otherwise = return $ M.insert name (Class (S.fromList supers) S.empty) ce

-- Add an instance of a superclass, with the given qualifiers.
-- Check that the superclass exists, and that there are no overlapping instances
addInstance :: MonadError String m => ClassInstance -> ClassEnvironment -> m ClassEnvironment
addInstance inst@(Qual _ (IsInstance classname _)) ce =
    case M.lookup classname ce of -- Find the class we're making an instance of
        Nothing -> throwError "Class doesn't exist"
        Just (Class supers otherInsts) -> do
            when (any (inst `overlaps`) otherInsts) (throwError "Overlapping instances")
            return $ M.insert classname (Class supers (S.insert inst otherInsts)) ce
    where
        -- Two instances overlap if there's a substitution which unifies their heads
        overlaps (Qual _ head1) (Qual _ head2) = isRight (mgu head1 head2)
          

-- If the given type predicate is true in the given class environment, then all the predicates returned from this
-- function are also true (obtained by considering all the superclasses)
ifPThenBySuper :: MonadError String m => ClassEnvironment -> TypePredicate -> m (S.Set TypePredicate)
ifPThenBySuper ce p@(IsInstance classname ty) = do
    supers <- S.toList <$> superclasses classname ce
    foldM mergeSupers (S.singleton p) supers
    where mergeSupers acc classname' = S.union acc <$> ifPThenBySuper ce (IsInstance classname' ty)

-- Same as above, but getting predicates by unifying with instances of the class
ifPThenByInstance :: MonadError String m => ClassEnvironment -> TypePredicate -> m (Maybe (S.Set TypePredicate))
ifPThenByInstance ce p@(IsInstance classname _) = do
    insts <- instances classname ce
    -- Pick the first non-Nothing value (as we can't have overlapping instances, this is the right instance)
    return $ msum (S.map tryMatchInstance insts)
    where 
        eitherToMaybe = either (const Nothing) Just
        tryMatchInstance (Qual qualifiers hd) = do -- Maybe monad
            subs <- eitherToMaybe (match hd p) -- Find a substitution
            -- The new predicates are the constraints on the matching instance
            Just $ applySub subs qualifiers


-- Determines if the given predicate can be deduced from the given assumptions and the class environment
entails :: MonadError String m => ClassEnvironment -> S.Set TypePredicate -> TypePredicate -> m Bool
entails ce assumps p = (||) <$> entailedBySuperset <*> entailedByInstance
    where
        -- Can this predicate be satisifed by the superclasses?
        entailedBySuperset = (p `S.member`) . S.unions <$> mapM (ifPThenBySuper ce) (S.toList assumps)
        -- Can this predicate be satisfied by unification with other instances of this class?
        entailedByInstance = ifPThenByInstance ce p >>= \case
            Nothing -> return False
            Just qualifiers -> allM (entails ce assumps) qualifiers