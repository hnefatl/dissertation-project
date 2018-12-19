{-# Language FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Typechecker.Types where

import Control.Monad.Except
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, foldl')
import Language.Haskell.Syntax as Syntax

import AlphaEq
import Names
import NameGenerator

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
-- A `TypeConstant` is composed of the name of the constructor being used, along with the kinds of any remaining
-- arguments and the list of applied types
data Type = TypeVar !TypeVariable
          | TypeConstant !TypeConstantName ![Kind] ![Type]
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
data TypePredicate = IsInstance !TypeConstantName !Type deriving (Eq, Ord)

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
    show (TypeConstant (TypeConstantName "[]") _ []) = "[]"
    show (TypeConstant (TypeConstantName "[]") _ [t]) = printf "[%s]" (show t)
    show (TypeConstant (TypeConstantName "[]") _ _) = error "compiler Error: Invalid type"
    show (TypeConstant (TypeConstantName "(,)") ks ts) = printf "(%s)" elements
        where elements = intercalate "," (map show ts ++ replicate (length ks) "")
    show (TypeConstant (TypeConstantName "->") _ ts) = case ts of
            [] -> "(->)"
            [t] -> printf "(%s ->)" (assocShow t)
            [arg, ret] -> printf "%s -> %s" (assocShow arg) (show ret)
            _ -> error "Compiler Error: Invalid type"
        where assocShow t@(TypeConstant (TypeConstantName "->") _ ts')
                | length ts' >= 2 = printf "(%s)" (show t)
                | otherwise = show t
              assocShow t = show t
    -- General show case
    show (TypeConstant (TypeConstantName name) _ []) = name
    show (TypeConstant (TypeConstantName name) _ ts) = printf "(%s %s)" name (unwords $ map show ts)
instance Show TypePredicate where
    show (IsInstance (TypeConstantName name) t@TypeVar{}) = printf "%s %s" name (show t)
    show (IsInstance (TypeConstantName name) t@TypeConstant{}) = printf "%s (%s)" name (show t)
instance Show a => Show (Qualified a) where
    show (Qualified quals x) = prefix ++ show x
        where prefix = case S.size quals of
                0 -> ""
                1 -> qualifiers ++ " => "
                _ -> printf "(%s) => " qualifiers
              qualifiers = intercalate ", " (map show $ S.toList quals)
instance Show QuantifiedType where
    show (Quantified quants t) = (if S.null quants then "" else quantifiers) ++ show t
        where quantifiers = printf "∀%s. " (intercalate ", " $ map show $ S.toList quants)


getInstantiatingTypeMap :: (MonadNameGenerator m, MonadError String m) => QuantifiedType -> m (M.Map TypeVariableName Type)
getInstantiatingTypeMap q = do
    m <- getInstantiatingMap q
    return $ M.map (\name -> TypeVar (TypeVariable name KindStar)) m

getInstantiatingMap :: (MonadNameGenerator m, MonadError String m) => QuantifiedType -> m (M.Map TypeVariableName TypeVariableName)
getInstantiatingMap (Quantified quants _) = M.fromList <$> mapM pairWithNewName (S.toList quants)
    where pairWithNewName (TypeVariable old _) = (old,) <$> freshTypeVarName

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
typeUnit = TypeConstant (TypeConstantName "()") [] []
typeBool = TypeConstant (TypeConstantName "Bool") [] []
typeInt = TypeConstant (TypeConstantName "Int") [] []
typeInteger = TypeConstant (TypeConstantName "Integer") [] []
typeFloat = TypeConstant (TypeConstantName "Float") [] []
typeDouble = TypeConstant (TypeConstantName "Double") [] []
typeChar = TypeConstant (TypeConstantName "Char") [] []

typeList, typeFun :: Type
typeList = TypeConstant (TypeConstantName "[]") [KindStar] []
typeFun = TypeConstant (TypeConstantName "->") [KindStar, KindStar] []

typeTuple :: Int -> Type
typeTuple n = TypeConstant (TypeConstantName "(,)") (replicate n KindStar) []

typeString :: Type
typeString = makeList typeChar


-- Utility functions for converting from our type representations to the AST representations and back
typeToSyn :: Type -> Syntax.HsType
typeToSyn (TypeVar (TypeVariable (TypeVariableName name) _)) = HsTyVar $ HsIdent name
typeToSyn (TypeConstant (TypeConstantName conName) _ ts) = case conName of
        "(,)" -> HsTyTuple $ map typeToSyn ts
        "->" -> case map typeToSyn ts of
                    [t1, t2] -> HsTyFun t1 t2
                    _ -> error "Should be parse error"
        name -> foldl' app (HsTyCon $ UnQual $ HsIdent name) ts
    where app x arg = HsTyApp x $ typeToSyn arg
synToType :: Syntax.HsType -> Type
synToType (HsTyFun arg body) = makeFun [synToType arg] $ synToType body
synToType (HsTyTuple ts) = makeTuple $ map synToType ts
synToType (HsTyApp t1 t2) = applyTypeUnsafe (synToType t1) (synToType t2)
synToType (HsTyVar v) = TypeVar $ TypeVariable (convertName v) KindStar
-- TODO(kc506): This is going to break fast when we use type constructors like `Maybe Int` as we don't know here that
-- the kind of `Maybe` is `* -> *`. Same goes for using `applyTypeUnsafe` above. These functions should be wrapped in a
-- `MonadError` at least to allow for `applyType`, and should be given access to information about what type constuctors
-- we have+what their kinds are, so we can validate if the type is valid. Moved into the typechecker monad?
synToType (HsTyCon c) = TypeConstant (convertName c) [] []

typePredToSyn :: TypePredicate -> Syntax.HsAsst
typePredToSyn (IsInstance (TypeConstantName c) t) = (UnQual $ HsIdent c, [typeToSyn t])
synToTypePred :: Syntax.HsAsst -> TypePredicate
synToTypePred (c, [t]) = IsInstance (convertName c) (synToType t)
synToTypePred _ = error "Invalid constraint (unary or multiparameter). Also, change this to throwError."

qualTypeToSyn :: QualifiedType -> Syntax.HsQualType
qualTypeToSyn (Qualified quals t) = HsQualType (map typePredToSyn $ S.toAscList quals) $ typeToSyn t
synToQualType :: Syntax.HsQualType -> QualifiedType
synToQualType (HsQualType quals t) = Qualified (S.fromList $ map synToTypePred quals) (synToType t)


instance AlphaEq TypeVariable where
    alphaEq' (TypeVariable n1 k1) (TypeVariable n2 k2) = (k1 == k2 &&) <$> alphaEq' n1 n2
instance AlphaEq Type where
    alphaEq' (TypeVar t1) (TypeVar t2) = alphaEq' t1 t2
    alphaEq' (TypeConstant n1 ks1 ts1) (TypeConstant n2 ks2 ts2) = do
        tsOkay <- and <$> zipWithM alphaEq' ts1 ts2
        return $ n1 == n2 && ks1 == ks2 && tsOkay
    alphaEq' _ _ = return False
instance AlphaEq TypePredicate where
    alphaEq' (IsInstance c1 t1) (IsInstance c2 t2) = (c1 == c2 &&) <$> alphaEq' t1 t2
instance AlphaEq a => AlphaEq (Qualified a) where
    alphaEq' (Qualified quals1 t1) (Qualified quals2 t2) = (&&) <$> alphaEq' t1 t2 <*> alphaEq' quals1 quals2
instance AlphaEq QuantifiedType where
    alphaEq' (Quantified quants1 t1) (Quantified quants2 t2) = (&&) <$> alphaEq' t1 t2 <*> alphaEq' quants1 quants2