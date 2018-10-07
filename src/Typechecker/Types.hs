{-# LANGUAGE FlexibleContexts #-}

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
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = return k
instance HasKind TypeConstant where
    getKind (TypeConstant _ k) = return k
instance HasKind Type where
    getKind (TypeVar var) = getKind var
    getKind (TypeConst con) = getKind con
    getKind (TypeQuant _) = throwError "Quantified type has no kind"
    getKind (TypeApp t _) = do
        kind <- getKind t
        case kind of
            KindFun _ k -> return k -- The kind of a type application is the kind of the output of the type function
            _ -> throwError "Type application has incorrect kind"


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

data TypeConstant = TypeConstant Id Kind deriving (Eq)
instance Show TypeConstant where
    show (TypeConstant name _) = name
instance Ord TypeConstant where
    compare (TypeConstant id1 _) (TypeConstant id2 _) = compare id1 id2


-- The possible types of haskell expressions
data Type = TypeVar TypeVariable
          | TypeConst TypeConstant
          | TypeApp Type Type
          | TypeQuant Int -- Quantified type variables: used in type schemes (forall a. Ord a => a)
    deriving (Eq, Show)

instance AssocShow Type where
    assocShow _ (TypeVar var) = show var
    assocShow _ (TypeConst con) = show con
    assocShow _ (TypeQuant n) = "q" ++ show n
    -- Type constructor special cases.
    assocShow False (TypeApp t@(TypeApp t1 t2) t3)
        | t1 == typeFun = assocShow True t2 ++ " -> " ++ assocShow False t3
        | t1 == typeTuple2 = "(" ++ assocShow False t2 ++ ", " ++ assocShow False t3 ++ ")"
        | otherwise = assocShow False t ++ " " ++ assocShow False t3
    assocShow False (TypeApp t1 t2)
        | t1 == typeList = "[" ++ assocShow False t2 ++ "]"
        | otherwise = assocShow False t1 ++ " " ++ assocShow False t2
    assocShow True t = "(" ++ assocShow False t ++ ")"


-- Utility functions for constructing types
makeList :: Type -> Type
makeList = TypeApp typeList

makeFun, makeTuple2 :: Type -> Type -> Type
makeFun = TypeApp . TypeApp typeFun
makeTuple2 = TypeApp . TypeApp typeTuple2

-- Built-in types
-- TODO(kc506): Find a better place to put these: somewhere along with their implementations?
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type
typeUnit = TypeConst (TypeConstant "()" KindStar)
typeBool = TypeConst (TypeConstant "Bool" KindStar)
typeInt = TypeConst (TypeConstant "Int" KindStar)
typeInteger = TypeConst (TypeConstant "Integer" KindStar)
typeFloat = TypeConst (TypeConstant "Float" KindStar)
typeDouble = TypeConst (TypeConstant "Double" KindStar)
typeChar = TypeConst (TypeConstant "Char" KindStar)

typeList, typeFun, typeTuple2 :: Type
typeList = TypeConst (TypeConstant "[]" (KindFun KindStar KindStar))
typeFun = TypeConst (TypeConstant "->" (KindFun KindStar (KindFun KindStar KindStar)))
typeTuple2 = TypeConst (TypeConstant "(,)" (KindFun KindStar (KindFun KindStar KindStar)))

typeString :: Type
typeString = makeList typeChar