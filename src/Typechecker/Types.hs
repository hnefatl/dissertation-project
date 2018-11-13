{-# Language FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Typechecker.Types where

import Control.Monad.Except
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, foldl')

import ExtraDefs

type TypeVariableName = Id
type VariableName = Id

-- |A kind is the "type of a type": either `*` or `Kind -> Kind`
-- Int has kind *, Maybe has kind * -> *, Either has kind * -> * -> *
data Kind = KindStar | KindFun !Kind !Kind deriving (Eq, Ord)

-- |The number of type arguments that a kind represents: this is one less than the number of nodes on the right-most
-- branch of a "Kind tree".
getKindArgCount :: Integral i => Kind -> i
getKindArgCount KindStar = 0
getKindArgCount (KindFun _ k) = 1 + getKindArgCount k

data TypeVariable = TypeVariable !TypeVariableName !Kind deriving (Eq, Ord)

getTvName :: TypeVariable -> TypeVariableName
getTvName (TypeVariable name _) = name

-- |The type of a Haskell expression.
-- A `TypeVariable` is a globally unique name for a type variable.
-- A `TypeDummy` is a locally (within the type) unique name for a type variable.
-- A `TypeConstant` is composed of the name of the constructor being used, along with the kinds of any remaining
-- arguments and the list of applied types
data Type = TypeVar !TypeVariable
          | TypeConstant !Id ![Kind] ![Type]
    deriving (Eq, Ord)

applyType :: MonadError String m => Type -> Type -> m Type
applyType (TypeConstant name (k:ks) ts) t
    | k == getKind t = return $ TypeConstant name ks (ts ++ [t])
    | otherwise = throwError $ printf "Got type of kind %s, expected kind %s" (show $ getKind t) (show k)
applyType (TypeConstant _ [] _) _ = throwError "Application of type of kind *"
-- TODO(kc506): This is valid (although maybe only with compiler extensions): `Monad m => a -> m a`
applyType t1 t2 = throwError $ printf "Can't apply type to a type variable - possible missing compiler feature: %s %s" (show t1) (show t2)

-- |"Unsafe" version of `applyType` which uses `error` instead of `throwError`: useful for compiler tests etc where
-- it's convenient to avoid boilerplate and we shouldn't have invalid types being used
applyTypeUnsafe :: Type -> Type -> Type
applyTypeUnsafe t1 t2 = case applyType t1 t2 of
    Left err -> error err
    Right t -> t

-- |A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeDummy "a" KindStar)`
-- Used in quite a few places: as constraints on types and in class/instance declarations, eg.
-- `foo :: Ord a => a`, `class Eq a => Ord a`, `instance Eq Int`, `instance Eq a => Eq [a]`, ...
data TypePredicate = IsInstance !TypeVariableName !Type deriving (Eq, Ord)

-- |A qualified thing: anywhere we can use `=>` is a qualified type, eg. `Eq a => Eq [a]` is a `Qualified
-- UninstantiatedType (TypePredicate UninstantiatedType)`, and `Eq a => a -> a -> a` is a `Qualified UninstantiatedType
-- UninstantiatedType`.
data Qualified a = Qualified !(S.Set TypePredicate) !a deriving (Eq, Ord)
-- |Some common applications of Qualified
type QualifiedType = Qualified Type
-- |A typeclass instance is eg. `instance Ord a => Ord [a]` or `instance Ord Int`.
type ClassInstance = Qualified TypePredicate

-- |A forall-quantified type: eg. `forall a. Ord a => a -> a -> Bool`
data QuantifiedType = Quantified !(S.Set TypeVariable) !QualifiedType deriving (Eq, Ord)

-- |A class for things that have a "kind": various type variable/constant/dummies, and types.
class HasKind t where
    -- |Returns the kind of the given `t`
    getKind :: t -> Kind
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = k
instance HasKind Type where
    getKind (TypeVar v) = getKind v
    -- The kind of an application is those kinds remaining in the application followed by a KindStar.
    getKind (TypeConstant _ kinds _) = foldr KindFun KindStar kinds

instance Show Kind where
    show = assocShow False
        where
        assocShow _ KindStar = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 ++ " -> " ++ assocShow False k2
        assocShow True k = "(" ++ assocShow False k ++ ")"
instance Show TypeVariable where
    show (TypeVariable name _) = show name
instance Show Type where
    show (TypeVar v) = show v

    -- Special show cases for builtin types
    show (TypeConstant (Id "[]") _ []) = "[]"
    show (TypeConstant (Id "[]") _ [t]) = printf "[%s]" (show t)
    show (TypeConstant (Id "[]") _ _) = error "compiler Error: Invalid type"
    show (TypeConstant (Id "(,)") ks ts) = printf "(%s)" elements
        where elements = intercalate "," (map show ts ++ replicate (length ks) "")
    show (TypeConstant (Id "->") _ ts) = case ts of
            [] -> "(->)"
            [t] -> printf "(%s ->)" (assocShow t)
            [arg, ret] -> printf "%s -> %s" (assocShow arg) (show ret)
            _ -> error "Compiler Error: Invalid type"
        where assocShow t@(TypeConstant (Id "->") _ ts') = if length ts' >= 2 then printf "(%s)" (show t) else show t
              assocShow t = show t
    -- General show case
    show (TypeConstant (Id name) _ []) = name
    show (TypeConstant (Id name) _ ts) = printf "(%s %s)" name (unwords $ map show ts)
instance Show TypePredicate where
    show (IsInstance (Id name) t) = name ++ " (" ++ show t ++ ")"
instance Show a => Show (Qualified a) where
    show (Qualified quals x) = "(" ++ qualifiers ++ ") => " ++ show x
        where qualifiers = intercalate ", " (map show $ S.toList quals)
instance Show QuantifiedType where
    show (Quantified quants t) = "forall " ++ quantifiers ++ ". " ++ show t
        where quantifiers = unwords (map show $ S.toList quants)


-- |Instantiate a quantified type into a qualified type, replacing all universally quantified variables with new
-- type variables.
instantiate :: (NameGenerator m Id, MonadError String m) => QuantifiedType -> m QualifiedType
instantiate qt@(Quantified _ (Qualified quals t)) = do
    varMap <- getInstantiatingTypeMap qt
    let instSet = S.fromList . map instPred . S.toList
        instPred (IsInstance classname x) = IsInstance classname (instType x)
        instType v@(TypeVar (TypeVariable name _)) = M.findWithDefault v name varMap
        instType (TypeConstant name ks ts) = TypeConstant name ks (map instType ts)
    return $ Qualified (instSet quals) (instType t)

getInstantiatingTypeMap :: (NameGenerator m Id, MonadError String m) => QuantifiedType -> m (M.Map TypeVariableName Type)
getInstantiatingTypeMap q = do
    m <- getInstantiatingMap q
    return $ M.map (\name -> TypeVar (TypeVariable name KindStar)) m

getInstantiatingMap :: (NameGenerator m Id, MonadError String m) => QuantifiedType -> m (M.Map TypeVariableName TypeVariableName)
getInstantiatingMap (Quantified quants _) = M.fromList <$> mapM pairWithNewName (S.toList quants)
    where pairWithNewName (TypeVariable old _) = (old,) <$> freshName

-- TODO(kc506): Find a better place to put these
-- |Utility functions on types
makeList :: Type -> Type
makeList = applyTypeUnsafe typeList
makeFun :: [Type] -> Type -> Type
makeFun args ret = foldr (applyTypeUnsafe . applyTypeUnsafe typeFun) ret args
makeTuple :: [Type] -> Type
makeTuple elements = foldl' applyTypeUnsafe (typeTuple $ length elements) elements

-- |Built-in types
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type
typeUnit = TypeConstant (Id "()") [] []
typeBool = TypeConstant (Id "Bool") [] []
typeInt = TypeConstant (Id "Int") [] []
typeInteger = TypeConstant (Id "Integer") [] []
typeFloat = TypeConstant (Id "Float") [] []
typeDouble = TypeConstant (Id "Double") [] []
typeChar = TypeConstant (Id "Char") [] []

typeList, typeFun :: Type
typeList = TypeConstant (Id "[]") [KindStar] []
typeFun = TypeConstant (Id "->") [KindStar, KindStar] []

typeTuple :: Int -> Type
typeTuple n = TypeConstant (Id "(,)") (replicate n KindStar) []

typeString :: Type
typeString = makeList typeChar