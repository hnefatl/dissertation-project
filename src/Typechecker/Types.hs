{-# Language FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Typechecker.Types where

import BasicPrelude hiding (intercalate)
import Data.Text (unpack)
import TextShow (TextShow, showt, showb, fromText)
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Language.Haskell.Syntax as Syntax

import Names
import NameGenerator

-- |A kind is the "type of a type": either `*` or `Kind -> Kind`
-- Int has kind *, Maybe has kind * -> *, Either has kind * -> * -> *
data Kind = KindStar | KindFun !Kind !Kind deriving (Eq, Ord)

data TypeVariable = TypeVariable !TypeVariableName !Kind deriving (Eq, Ord)
data TypeConstant = TypeConstant !TypeVariableName !Kind deriving (Eq, Ord)

-- |The type of a Haskell expression.
-- A TypeVar is a type variable, a TypeCon is a type constant, and a TypeApp is the application of one type to another
-- and the resulting Kind.
data Type = TypeVar !TypeVariable
          | TypeCon !TypeConstant
          | TypeApp !Type !Type !Kind
    deriving (Eq, Ord)

-- |Type-level function application, as in `Maybe` applied to `Int` gives `Maybe Int`.
applyTypeFun :: MonadError Text m => Type -> Type -> m Type
applyTypeFun t1 t2 = case getKind t1 of
    KindStar -> throwError $ "Application to type of kind *: " <> showt t1 <> " applied to " <> showt t2
    KindFun argKind resKind
        | argKind == getKind t2 -> return $ TypeApp t1 t2 resKind
        | otherwise -> throwError $ "Kind mismatch: " <> showt t1 <> " applied to " <> showt t2

-- |"Unsafe" version of `applyTypeFun` which uses `error` instead of `throwError`: useful for compiler tests etc where
-- it's convenient to avoid boilerplate and we shouldn't have invalid types being used
applyTypeFunUnsafe :: Type -> Type -> Type
applyTypeFunUnsafe t1 t2 = case applyTypeFun t1 t2 of
    Left err -> error (unpack err)
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
data Quantified a = Quantified !(S.Set TypeVariable) !a deriving (Eq, Ord)
type QuantifiedType = Quantified QualifiedType
type QuantifiedSimpleType = Quantified Type

-- |A class for things that have a "kind": type variables/constants, types, ...
class HasKind t where
    -- |Returns the kind of the given `t`
    getKind :: t -> Kind
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = k
instance HasKind TypeConstant where
    getKind (TypeConstant _ k) = k
instance HasKind Type where
    getKind (TypeVar v) = getKind v
    getKind (TypeCon c) = getKind c
    getKind (TypeApp _ _ k) = k

instance TextShow Kind where
    showb = assocShow False
        where
        assocShow _ KindStar = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 <> " -> " <> assocShow False k2
        assocShow True k = "(" <> assocShow False k <> ")"
instance TextShow TypeVariable where
    showb (TypeVariable name _) = showb name
instance TextShow TypeConstant where
    showb (TypeConstant name _) = showb name
instance TextShow Type where
    showb (TypeVar v) = showb v
    showb (TypeCon c) = showb c
    showb (TypeApp t1 t2 _) = case t1 of
        TypeCon (TypeConstant (TypeVariableName "[]") _) -> "[" <> showb t2 <> "]"
        TypeApp (TypeCon (TypeConstant (TypeVariableName "->") _)) t3 _ -> case t3 of
            TypeApp{} -> "(" <> showb t3 <> ") -> " <> showb t2
            _ -> showb t3 <> " -> " <> showb t2
        _ -> showb t1 <> " " <> showb t2

instance TextShow TypePredicate where
    showb (IsInstance (TypeVariableName name) t@TypeApp{}) = fromText name <> "(" <> showb t <> ")"
    showb (IsInstance (TypeVariableName name) t) = fromText name <> " " <> showb t
instance TextShow a => TextShow (Qualified a) where
    showb (Qualified quals x) = prefix <> showb x
        where prefix = case S.size quals of
                0 -> ""
                1 -> qualifiers <> " => "
                _ -> "(" <> qualifiers <> ") => "
              qualifiers = mconcat $ intersperse ", " $ map showb $ S.toList quals
instance TextShow a => TextShow (Quantified a) where
    showb (Quantified quants t) = (if S.null quants then "" else quantifiers) <> showb t
        where quantifiers = "∀" <> (mconcat $ intersperse ", " $ map showb $ S.toList quants) <> ". "


getInstantiatingTypeMap :: (MonadNameGenerator m, MonadError Text m) => QuantifiedType -> m (M.Map TypeVariableName Type)
getInstantiatingTypeMap q = do
    m <- getInstantiatingMap q
    return $ M.map (\name -> TypeVar (TypeVariable name KindStar)) m

getInstantiatingMap :: (MonadNameGenerator m, MonadError Text m) => QuantifiedType -> m (M.Map TypeVariableName TypeVariableName)
getInstantiatingMap (Quantified quants _) = M.fromList <$> mapM pairWithNewName (S.toList quants)
    where pairWithNewName (TypeVariable old _) = (old,) <$> freshTypeVarName

mergeQuantifiedTypes :: ([Type] -> Type) -> [QuantifiedType] -> QuantifiedType
mergeQuantifiedTypes f qts = Quantified (S.unions quants) $ Qualified (S.unions quals) (f ts)
    where (quants, quals, ts) = unzip3 $ map (\(Quantified quant (Qualified qual t)) -> (quant, qual, t)) qts

-- TODO(kc506): Find a better place to put these
-- |Utility functions on types
makeList :: Type -> Type
makeList = applyTypeFunUnsafe typeList
makeFun :: [Type] -> Type -> Type
makeFun args ret = foldr (applyTypeFunUnsafe . applyTypeFunUnsafe typeFun) ret args
makeTuple :: [Type] -> Type
makeTuple elements = foldl' applyTypeFunUnsafe (typeTuple $ length elements) elements

-- |Given a type representing a function, unpack it to return the arguments and return type
unmakeFun :: MonadError Text m => Type -> m ([Type], Type)
unmakeFun t = do
    let helper x = case unwrapFunMaybe x of
            Nothing -> ([], x)
            Just (t1, t') -> (t1:ts, t'')
                where (ts, t'') = helper t'
    (t0, t') <- unwrapFun t
    let (ts, t'') = helper t'
    return (t0:ts, t'')
-- |Unwrapping a constructor is less restrictive than unwrapping a function: we don't necessarily need any arguments
unmakeCon :: Type -> ([Type], Type)
unmakeCon t = either (const ([], t)) id (unmakeFun t)

-- |Split a type representing a function into the argument and the return type
unwrapFunMaybe :: Type -> Maybe (Type, Type)
unwrapFunMaybe (TypeApp (TypeApp f t1 _) t2 _)
    | f == typeFun = Just (t1, t2)
    | otherwise = Nothing
unwrapFunMaybe _ = Nothing
unwrapFun :: MonadError Text m => Type -> m (Type, Type)
unwrapFun t = maybe (throwError $ showt t <> " isn't a function type.") return (unwrapFunMaybe t)

unmakeList :: MonadError Text m => Type -> m Type
unmakeList t@(TypeApp t1 t2 _)
    | t1 == typeList = return t2
    | otherwise = throwError $ showt t <> " isn't a list type"
unmakeList t = throwError $ showt t <> " isn't a list type"

-- |Built-in types
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type
typeUnit = TypeCon $ TypeConstant (TypeVariableName "()") KindStar
typeBool = TypeCon $ TypeConstant (TypeVariableName "Bool") KindStar
typeInt = TypeCon $ TypeConstant (TypeVariableName "Int") KindStar
typeInteger = TypeCon $ TypeConstant (TypeVariableName "Integer") KindStar
typeFloat = TypeCon $ TypeConstant (TypeVariableName "Float") KindStar
typeDouble = TypeCon $ TypeConstant (TypeVariableName "Double") KindStar
typeChar = TypeCon $ TypeConstant (TypeVariableName "Char") KindStar

typeList, typeFun :: Type
typeList = TypeCon $ TypeConstant (TypeVariableName "[]") (KindFun KindStar KindStar)
typeFun = TypeCon $ TypeConstant (TypeVariableName "->") (KindFun KindStar $ KindFun KindStar KindStar)

typeTuple :: Int -> Type
typeTuple n = TypeCon $ TypeConstant (TypeVariableName "(,)") (foldr KindFun KindStar $ replicate n KindStar)

typeString :: Type
typeString = makeList typeChar


-- Utility functions for converting from our type representations to the AST representations and back
typeToSyn :: MonadError Text m => Type -> m Syntax.HsType
typeToSyn (TypeVar (TypeVariable (TypeVariableName name) _)) = return $ HsTyVar $ HsIdent $ unpack name
typeToSyn (TypeCon (TypeConstant (TypeVariableName name) _)) = return $ case name of
    "[]" -> HsTyCon $ Special HsListCon
    ":" -> HsTyCon $ Special HsCons
    "()" -> HsTyCon $ Special HsUnitCon
    "->" -> HsTyCon $ Special HsFunCon
    "(,)" -> HsTyCon $ Special $ HsTupleCon 0 -- TODO(kc506): Count commas, output specific type?
    _ -> HsTyCon $ UnQual $ HsIdent $ unpack name
typeToSyn (TypeApp t1 t2 _) = do
    t1' <- typeToSyn t1
    t2' <- typeToSyn t2
    return $ case t1' of
        HsTyApp (HsTyCon (Special HsFunCon)) t3 -> HsTyFun t3 t2'
        HsTyTuple ts -> HsTyTuple (ts ++ [t2'])
        HsTyCon (Special (HsTupleCon _)) -> HsTyTuple [t2']
        _ -> HsTyApp t1' t2'
synToType :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsType -> m Type
synToType _ (HsTyVar v) = return $ TypeVar $ TypeVariable (convertName v) KindStar
synToType kinds (HsTyCon c) = case M.lookup (TypeVariableName name) kinds of
    Nothing -> throwError $ "Type constructor not in kind mapping: " <> showt name
    Just kind -> return $ TypeCon $ TypeConstant (TypeVariableName name) kind
    where name = convertName c
synToType ks (HsTyFun arg body) = makeFun <$> sequence [synToType ks arg] <*> synToType ks body
synToType ks (HsTyTuple ts) = makeTuple <$> mapM (synToType ks) ts
synToType ks (HsTyApp t1 t2) = do
    t1' <- synToType ks t1
    t2' <- synToType ks t2
    applyTypeFun t1' t2'

typePredToSyn :: MonadError Text m => TypePredicate -> m Syntax.HsAsst
typePredToSyn (IsInstance (TypeVariableName c) t) = do
    t' <- typeToSyn t
    return (UnQual $ HsIdent $ unpack c, [t'])
synToTypePred :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsAsst -> m TypePredicate
synToTypePred ks (c, [t]) = IsInstance (convertName c) <$> synToType ks t
synToTypePred _ _ = throwError "Invalid constraint (unary or multiparameter)."

qualTypeToSyn :: MonadError Text m => QualifiedType -> m Syntax.HsQualType
qualTypeToSyn (Qualified quals t) = HsQualType <$> mapM typePredToSyn (S.toAscList quals) <*> typeToSyn t
synToQualType :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsQualType -> m QualifiedType
synToQualType ks (HsQualType quals t) = Qualified <$> (S.fromList <$> mapM (synToTypePred ks) quals) <*> synToType ks t