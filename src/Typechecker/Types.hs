module Typechecker.Types where

-- General variable/type name
type Id = String

makeVariableName :: Int -> Id
makeVariableName i = "var" ++ show i

class HasKind t where
    getKind :: t -> Kind
instance HasKind Kind where
    getKind = id
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = k
instance HasKind TypeConstant where
    getKind (TypeConstant _ k) = k
instance HasKind Type where
    getKind (TypeVar var) = getKind var
    getKind (TypeConst con) = getKind con
    getKind (TypeQuant _) = error "Quantified type has no kind"
    getKind (TypeApp t _) = case getKind t of
        KindFun _ k -> k -- The kind of a type application is the kind of the output of the type function
        _ -> error "Type application has incorrect kind"


-- A kind is either `*` or `Kind -> Kind`
data Kind = KindStar | KindFun Kind Kind
    deriving (Eq)
instance Show Kind where
    show KindStar = "*"
    show (KindFun k@(KindFun _ _) k3) = "(" ++ show k ++ ") -> " ++ show k3
    show (KindFun k1 k2) = show k1 ++ " -> " ++ show k2


data TypeVariable = TypeVariable Id Kind deriving (Eq)
data TypeConstant = TypeConstant Id Kind deriving (Eq)

-- The possible types of haskell expressions
data Type = TypeVar TypeVariable
          | TypeConst TypeConstant
          | TypeApp Type Type
          | TypeQuant Int -- Quantified type variables: used in type schemes (forall a. Ord a => a)
    deriving (Eq)

instance Show Type where
    show (TypeVar (TypeVariable name _)) = show name
    show (TypeConst (TypeConstant name _)) = show name
    show (TypeQuant n) = "q" ++ show n
    -- Type constructor special cases. TODO(kc506): find a nice way to match functions
    show (TypeApp t1 t2) | t1 == typeList = "[" ++ show t2 ++ "]"
                         | otherwise = show t1 ++ " " ++ show t2

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

typeList, typeFunction, typeTuple2 :: Type
typeList = TypeConst (TypeConstant "[]" (KindFun KindStar KindStar))
typeFunction = TypeConst (TypeConstant "->" (KindFun KindStar (KindFun KindStar KindStar)))
typeTuple2 = TypeConst (TypeConstant "(,)" (KindFun KindStar (KindFun KindStar KindStar)))

typeString :: Type
typeString = TypeApp typeList typeChar