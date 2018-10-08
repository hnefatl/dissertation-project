{-# Language FlexibleContexts, GADTs #-}

module Typechecker.Types where

import Control.Monad.Except

-- General variable/type name
type Id = String

makeVariableName :: Int -> Id
makeVariableName i = "var" ++ show i

class HasKind t where
    getKind :: MonadError String m => t -> m Kind
instance HasKind Kind where
    getKind = return


-- A class for types that can be "shown" but have special behaviour based on associativity to prevent ambiguity
-- Eg. "(* -> *) -> *" is distinct from "* -> * -> *"
-- Assumes all instances are right-associative for ease of implementation
class AssocShow t where
    assocShow :: Bool -> t -> String -- Bool flag indicates if we should explicitly show associativity

-- A kind is either `*` or `Kind -> Kind`
data Kind = KindStar | KindFun Kind Kind
    deriving (Eq, Show)
instance AssocShow Kind where
    assocShow _ KindStar = "*"
    assocShow False (KindFun k1 k2) = assocShow True k1 ++ " -> " ++ assocShow False k2
    assocShow True k = "(" ++ assocShow False k ++ ")"


data TypeVariable = TypeVariable Id Kind deriving (Eq)
instance Show TypeVariable where
    show (TypeVariable name _) = name
instance Ord TypeVariable where
    compare (TypeVariable id1 _) (TypeVariable id2 _) = compare id1 id2
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = return k

data TypeConstant = TypeConstant Id Kind deriving (Eq)
instance Show TypeConstant where
    show (TypeConstant name _) = name
instance Ord TypeConstant where
    compare (TypeConstant id1 _) (TypeConstant id2 _) = compare id1 id2
instance HasKind TypeConstant where
    getKind (TypeConstant _ k) = return k


-- Represents the type of a Haskell expression. The type parameter allows for customisation of the value stored
-- for a type variable
data Type t = TypeVar t
            | TypeConst TypeConstant
            | TypeApp (Type t) (Type t)
    deriving (Eq, Show)

instance Functor Type where
    fmap f (TypeVar x) = TypeVar (f x)
    fmap _ (TypeConst c) = TypeConst c -- Unwrap then rewrap to change type from `f a` to `f b`
    fmap f (TypeApp x y) = TypeApp (fmap f x) (fmap f y)
instance Foldable Type where
    foldMap f (TypeVar x) = f x
    foldMap _ (TypeConst _) = mempty
    foldMap f (TypeApp x y) = mappend (foldMap f x) (foldMap f y)
instance Traversable Type where
    traverse f (TypeVar x) = TypeVar <$> f x
    traverse _ (TypeConst c) = pure (TypeConst c)
    traverse f (TypeApp x y) = TypeApp <$> traverse f x <*> traverse f y


instance HasKind a => HasKind (Type a) where
    getKind (TypeVar x) = getKind x
    getKind (TypeConst con) = getKind con
    getKind (TypeApp t _) = do
        kind <- getKind t
        case kind of
            KindFun _ k -> return k -- The kind of a type application is the kind of the output of the type function
            _ -> throwError "Type application has incorrect kind"

-- A type with named variables
type ConcreteType = Type TypeVariable
-- A type with un-named variables
type QuantifiedType = Type Kind

-- Convert a type with dummy variables into one with (uniquely) named variables
-- nameGenerator should return a new unique id each time it's run
realise :: Monad m => m Id -> QuantifiedType -> m ConcreteType
realise nameGenerator = mapM convert
    where convert kind = TypeVariable <$> nameGenerator <*> pure kind


instance (Eq a, Show a) => AssocShow (Type a) where
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