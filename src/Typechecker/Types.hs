{-# Language FlexibleContexts, LambdaCase, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Typechecker.Types where

import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate)

-- |General variable/type name
type Id = String

-- |A kind is the "type of a type": either `*` or `Kind -> Kind`
-- Int has kind *, Maybe has kind * -> *, Either has kind * -> * -> *
data Kind = KindStar | KindFun !Kind !Kind deriving (Eq)

-- A type variable is a named variable tagged with a kind: eg. in the type expression `a b` we'd extract
-- `TypeVariable "a" (KindFun KindStar KindStar)` and `TypeVariable "b" KindStar`
data TypeVariable = TypeVariable !Id !Kind
-- A type constant is a non-substituable symbol in a type: eg. `Maybe a` gives
-- `TypeConstant "Maybe" (KindFun Star Star)` and `TypeVariable "a" KindStar`.
data TypeConstant = TypeConstant !Id !Kind
-- A dummy type variable: we can substitute on TypeVariables safely as their names should be "global", but a dummy type
-- variable has "local" names so isn't safe to substitute on (unrelated variables in different types may have the same
-- name).
data TypeDummy = TypeDummy !Id !Kind

-- The type of a Haskell expression. Parameter allows for customisation of the value stored for a type variable
data Type t = TypeVar !t
            | TypeConst !TypeConstant
            | TypeApp !(Type t) !(Type t)
    deriving (Eq, Ord, Functor, Foldable, Traversable)

-- Type with sanitised variable names (safe to substitute on)
-- The need for these two types is subtle: say we're given constraints `Eq Int` and `Eq a => Eq [a]` and asked to check
-- if `[[Int]]` is an instance of `Eq`. If we initially unify `a` with `[Int]` then we ruin later substitutions as
-- we've polluted `a`. Instead, we call the constraint an uninstantiated constraint, and require it to be instantiated
-- such that the type variable names are unique across the typechecker before unifying. We'd then unify eg. `a0` with
--`[Int]`, `a1` with `Int`, and we're done.
type InstantiatedType = Type TypeVariable
-- "Local" type that's not safe to substitute on
type UninstantiatedType = Type TypeDummy

-- A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeVar (TypeVariable "a" KindStar))`
-- Used in quite a few places: as constraints on types and in class/instance declarations, eg.
-- `foo :: Ord a => a`, `class Eq a => Ord a`, `instance Eq Int`, `instance Eq a => Eq [a]`, ...
data TypePredicate t = IsInstance !Id !t deriving (Eq, Ord, Functor, Foldable, Traversable)
-- Some common applications of type predicates
type InstantiatedTypePredicate = TypePredicate InstantiatedType
type UninstantiatedTypePredicate = TypePredicate UninstantiatedType

-- A qualified thing: anywhere we can use `=>` is a qualified type, eg. `Eq a => Eq [a]` is a `Qualified
-- UninstantiatedType (TypePredicate UninstantiatedType)`, and `Eq a => a -> a -> a` is a `Qualified UninstantiatedType
-- UninstantiatedType`.
data Qualified t a = Qualified !(S.Set (TypePredicate t)) !a deriving (Eq, Ord)
-- Some common applications of Qualified
type QualifiedType = Qualified InstantiatedType InstantiatedType
type UninstantiatedQualifiedType = Qualified UninstantiatedType UninstantiatedType


instance Show TypeVariable where show (TypeVariable name _) = name
instance Show TypeConstant where show (TypeConstant name _) = name
instance Show TypeDummy where show (TypeDummy name _) = name
instance Ord TypeVariable where compare (TypeVariable id1 _) (TypeVariable id2 _) = compare id1 id2
instance Ord TypeConstant where compare (TypeConstant id1 _) (TypeConstant id2 _) = compare id1 id2
instance Ord TypeDummy where compare (TypeDummy id1 _) (TypeDummy id2 _) = compare id1 id2
instance Eq TypeVariable where TypeVariable id1 _ == TypeVariable id2 _ = id1 == id2
instance Eq TypeConstant where TypeConstant id1 _ == TypeConstant id2 _ = id1 == id2
instance Eq TypeDummy where TypeDummy id1 _ == TypeDummy id2 _ = id1 == id2

instance (Show t, Show a) => Show (Qualified t a) where
    -- Handle eg. `Eq a => ...` vs `(Eq a, Show a) => ...`
    show (Qualified quals x) = pquals ++ " => " ++ show x
        where
            qualifiers = intercalate ", " (map show $ S.toList quals)
            pquals = if S.size quals > 1 then "(" ++ qualifiers ++ ")" else qualifiers


-- A class for things that have a "kind": kinds themselves, various type variable/constant/dummies, and types.
class HasKind t where
    getKind :: MonadError String m => t -> m Kind
instance HasKind Kind where
    getKind = return
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = return k
instance HasKind TypeConstant where
    getKind (TypeConstant _ k) = return k
instance HasKind a => HasKind (Type a) where
    getKind (TypeVar x) = getKind x
    getKind (TypeConst con) = getKind con
    getKind (TypeApp t _) = getKind t >>= \case
        KindFun _ k -> return k -- The kind of a type application is the kind of the output of the type function
        _ -> throwError "Type application has incorrect kind"


instance Show Kind where
    show = assocShow False
        where
        assocShow _ KindStar = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 ++ " -> " ++ assocShow False k2
        assocShow True k = "(" ++ assocShow False k ++ ")"
instance (Eq a, Show a) => Show (Type a) where
    show = assocShow False
        where
        assocShow _ (TypeVar var) = show var
        assocShow _ (TypeConst con) = show con
        -- Type constructor special cases.
        assocShow False (TypeApp t@(TypeApp t1 t2) t3)
            | t1 == typeFun = assocShow True t2 ++ " -> " ++ assocShow False t3
            | t1 == typeTuple2 = "(" ++ assocShow False t2 ++ ", " ++ assocShow False t3 ++ ")"
            | otherwise = assocShow False t ++ " " ++ assocShow False t3
        assocShow False (TypeApp t1 t2)
            | t1 == typeList = "[" ++ assocShow False t2 ++ "]"
            | otherwise = assocShow False t1 ++ " " ++ assocShow False t2
        assocShow True t = "(" ++ assocShow False t ++ ")"
instance Show t => Show (TypePredicate t) where
    show (IsInstance name t) = name ++ " (" ++ show t ++ ")"



-- A monad that can convert a type with dummy variables into one with (uniquely) named variables
class Monad m => TypeInstantiator m where
    -- Should generate a new unique name each time it's run
    freshName :: m Id

    -- Utility state monad function: given a local name, convert it to a global name, ensuring that matching local
    -- names map to the same global name
    freshNameMap :: Id -> StateT (M.Map Id Id) m Id
    freshNameMap localName = gets (M.lookup localName) >>= \case
            Nothing -> do
                globalName <- lift freshName
                state $ \s -> (globalName, M.insert localName globalName s)
            Just globalName -> return globalName
            
    instantiateType :: UninstantiatedQualifiedType -> m QualifiedType
    instantiateType t = evalStateT (instantiate freshNameMap t) M.empty

-- Class of things that can be instantiated (from dummy variables to global variables) from a to b
class Instantiable a b where
    instantiate :: Monad m => (Id -> m Id) -> a -> m b

instance Instantiable UninstantiatedType InstantiatedType where
    instantiate f = mapM (\(TypeDummy name kind) -> TypeVariable <$> f name <*> pure kind)
instance Instantiable (TypePredicate UninstantiatedType) (TypePredicate InstantiatedType) where
    instantiate f (IsInstance super t) = IsInstance super <$> instantiate f t
instance Instantiable UninstantiatedQualifiedType QualifiedType where
    instantiate f (Qualified quals t) = Qualified <$> quals' <*> instantiate f t
        where quals' = S.fromList <$> mapM (instantiate f) (S.toList quals)

    

-- Utility functions on types
makeList :: Type a -> Type a
makeList = TypeApp typeList
makeFun, makeTuple2 :: Type a -> Type a -> Type a
makeFun = TypeApp . TypeApp typeFun
makeTuple2 = TypeApp . TypeApp typeTuple2


-- Built-in types
-- TODO(kc506): Find a better place to put these: somewhere along with their implementations?
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type a
typeUnit = TypeConst (TypeConstant "()" KindStar)
typeBool = TypeConst (TypeConstant "Bool" KindStar)
typeInt = TypeConst (TypeConstant "Int" KindStar)
typeInteger = TypeConst (TypeConstant "Integer" KindStar)
typeFloat = TypeConst (TypeConstant "Float" KindStar)
typeDouble = TypeConst (TypeConstant "Double" KindStar)
typeChar = TypeConst (TypeConstant "Char" KindStar)

typeList, typeFun, typeTuple2 :: Type a
typeList = TypeConst (TypeConstant "[]" (KindFun KindStar KindStar))
typeFun = TypeConst (TypeConstant "->" (KindFun KindStar (KindFun KindStar KindStar)))
typeTuple2 = TypeConst (TypeConstant "(,)" (KindFun KindStar (KindFun KindStar KindStar)))

typeString :: Type a
typeString = makeList typeChar