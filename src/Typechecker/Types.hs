{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Typechecker.Types where

import           BasicPrelude               hiding (intercalate)
import           Control.DeepSeq            (NFData)
import           Control.Monad.Except       (MonadError, throwError)
import           Control.Monad.State.Strict (MonadState, evalStateT, gets, modify)
import           Data.Foldable              (foldlM, foldrM)
import           Data.Hashable              (Hashable)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Text                  (unpack)
import           GHC.Generics               (Generic)
import           Language.Haskell.Syntax    as Syntax
import           TextShow                   (TextShow, fromText, showb, showt)

import           ExtraDefs                  (synPrint)
import           NameGenerator
import           Names
import           Tuples                     (makeTupleName)

-- |A kind is the "type of a type": either `*` or `Kind -> Kind`
-- Int has kind *, Maybe has kind * -> *, Either has kind * -> * -> *
data Kind = KindStar | KindFun !Kind !Kind deriving (Eq, Ord, Show, Generic)
instance NFData Kind
instance Hashable Kind

data TypeVariable = TypeVariable !TypeVariableName !Kind deriving (Eq, Ord, Show, Generic)
data TypeConstant = TypeConstant !TypeVariableName !Kind deriving (Eq, Ord, Show, Generic)
instance NFData TypeVariable
instance NFData TypeConstant
instance Hashable TypeVariable
instance Hashable TypeConstant

-- |The type of a Haskell expression.
-- A TypeVar is a type variable, a TypeCon is a type constant, and a TypeApp is the application of one type to another
-- and the resulting Kind.
data Type = TypeVar !TypeVariable
          | TypeCon !TypeConstant
          | TypeApp !Type !Type !Kind
    deriving (Eq, Ord, Show, Generic)
instance NFData Type
instance Hashable Type

-- |Type-level function application, as in `Maybe` applied to `Int` gives `Maybe Int`.
applyTypeFun :: MonadError Text m => Type -> Type -> m Type
applyTypeFun t1 t2 =
    let errMsg = "Kind mismatch: " <> showt t1 <> " " <> showt t2
    in case kind t1 of
        KindStar -> throwError errMsg
        KindFun argKind resKind
            | argKind == kind t2 -> return $ TypeApp t1 t2 resKind
            | otherwise -> throwError errMsg

-- |"Unsafe" version of `applyTypeFun` which uses `error` instead of `throwError`: useful for compiler tests etc where
-- it's convenient to avoid boilerplate and we shouldn't have invalid types being used
applyTypeFunUnsafe :: Type -> Type -> Type
applyTypeFunUnsafe t1 t2 = case applyTypeFun t1 t2 of
    Left err -> error (unpack err)
    Right t  -> t

unmakeKindFun :: MonadError Text m => Kind -> m ([Kind], Kind)
unmakeKindFun (KindFun k1 KindStar) = return ([k1], KindStar)
unmakeKindFun (KindFun k1 k2) = do
    (argKinds, baseKind) <- unmakeKindFun k2
    return (k1:argKinds, baseKind)
unmakeKindFun KindStar = throwError "unmakeKindFun given KindStar"

-- |A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeDummy "a" KindStar)`
-- Used in quite a few places: as constraints on types and in class/instance declarations, eg.
-- `foo :: Ord a => a`, `class Eq a => Ord a`, `instance Eq Int`, `instance Eq a => Eq [a]`, ...
data TypePredicate = IsInstance !ClassName !Type deriving (Eq, Ord, Show, Generic)
instance NFData TypePredicate

-- |A qualified thing: anywhere we can use `=>` is a qualified type, eg. `Eq a => Eq [a]` is a `Qualified
-- UninstantiatedType (TypePredicate UninstantiatedType)`, and `Eq a => a -> a -> a` is a `Qualified UninstantiatedType
-- UninstantiatedType`.
data Qualified a = Qualified !(S.Set TypePredicate) !a deriving (Eq, Ord, Show, Generic)
instance NFData a => NFData (Qualified a)
-- |Some common applications of Qualified
type QualifiedType = Qualified Type
-- |A typeclass instance is eg. `instance Ord a => Ord [a]` or `instance Ord Int`.
type ClassInstance = Qualified TypePredicate

-- |A forall-quantified type: eg. `forall a. Ord a => a -> a -> Bool`
data Quantified a = Quantified !(S.Set TypeVariable) !a deriving (Eq, Ord, Show, Generic)
instance NFData a => NFData (Quantified a)
type QuantifiedType = Quantified QualifiedType
type QuantifiedSimpleType = Quantified Type

-- |A class for things that have a "kind": type variables/constants, types, ...
class HasKind t where
    -- |Returns the kind of the given `t`
    kind :: t -> Kind
instance HasKind TypeVariable where
    kind (TypeVariable _ k) = k
instance HasKind TypeConstant where
    kind (TypeConstant _ k) = k
instance HasKind Type where
    kind (TypeVar v)     = kind v
    kind (TypeCon c)     = kind c
    kind (TypeApp _ _ k) = k

instance TextShow Kind where
    showb = assocShow False
        where
        assocShow _ KindStar            = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 <> " -> " <> assocShow False k2
        assocShow True k                = "(" <> assocShow False k <> ")"
instance TextShow TypeVariable where
    showb (TypeVariable name KindStar) = showb name
    showb (TypeVariable name k)        = "(" <> showb name <> " :: " <> showb k <> ")"
instance TextShow TypeConstant where
    showb (TypeConstant name KindStar) = showb name
    showb (TypeConstant name k)        = "(" <> showb name <> " :: " <> showb k <> ")"
instance TextShow Type where
    showb (TypeVar v) = showb v
    showb (TypeCon c) = showb c
    showb (TypeApp t1 t2 _) = case t1 of
        TypeCon (TypeConstant "[]" _) -> "[" <> showb t2 <> "]"
        TypeApp (TypeCon (TypeConstant "->" _)) t3 _ -> case t3 of
            TypeApp{} -> "(" <> showb t3 <> ") -> " <> showb t2
            _         -> showb t3 <> " -> " <> showb t2
        _ -> showb t1 <> " " <> showb t2

instance TextShow TypePredicate where
    showb (IsInstance (TypeVariableName name) t@TypeApp{}) = fromText name <> " (" <> showb t <> ")"
    showb (IsInstance (TypeVariableName name) t)           = fromText name <> " " <> showb t
instance TextShow a => TextShow (Qualified a) where
    showb (Qualified quals x) = prefix <> showb x
        where prefix = case S.size quals of
                0 -> ""
                1 -> qualifiers <> " => "
                _ -> "(" <> qualifiers <> ") => "
              qualifiers = mconcat $ intersperse ", " $ map showb $ S.toList quals
instance TextShow a => TextShow (Quantified a) where
    showb (Quantified quants t)
        | S.null quants = showb t
        | otherwise = quantifiers <> showb t
            where quantifiers = "∀" <> mconcat (intersperse ", " $ map showb $ S.toList quants) <> ". "

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
makeFunUnsafe :: [Type] -> Type -> Type
makeFunUnsafe args ret = foldr (applyTypeFunUnsafe . applyTypeFunUnsafe typeFun) ret args
makeFun :: MonadError Text m => [Type] -> Type -> m Type
makeFun args ret = foldrM (\arg r -> makeApp typeFun [arg, r]) ret args
makeApp :: MonadError Text m => Type -> [Type] -> m Type
makeApp = foldlM applyTypeFun
makeListUnsafe :: Type -> Type
makeListUnsafe = applyTypeFunUnsafe typeList
makeList :: MonadError Text m => Type -> m Type
makeList = applyTypeFun typeList
makeTupleUnsafe :: [Type] -> Type
makeTupleUnsafe elements = foldl' applyTypeFunUnsafe (typeTuple $ length elements) elements
makeTuple :: MonadError Text m => [Type] -> m Type
makeTuple elements = foldlM applyTypeFun (typeTuple $ length elements) elements
makeSynFun :: [HsType] -> HsType -> HsType
makeSynFun as e = foldr HsTyFun e as
makeSynApp :: HsType -> [HsType] -> HsType
makeSynApp = foldl HsTyApp

synFunArgNum :: HsType -> Int
synFunArgNum (HsTyFun _ t) = 1 + synFunArgNum t
synFunArgNum _             = 0

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
-- |Deconstruct a Haskell AST function
unmakeSynFun :: HsType -> ([HsType], HsType)
unmakeSynFun (HsTyFun a e) = let (as, b) = unmakeSynFun e in (a:as, b)
unmakeSynFun t             = ([], t)

unmakeApp :: Type -> (Type, [Type])
unmakeApp (TypeApp t1 t2 _) = (baseT, ts ++ [t2])
    where (baseT, ts) = unmakeApp t1
unmakeApp t = (t, [])
unmakeSynApp :: HsType -> (HsType, [HsType])
unmakeSynApp (HsTyApp t1 t2) = (base, args <> [t2])
    where (base, args) = unmakeSynApp t1
unmakeSynApp t = (t, [])

-- |Split a type representing a function into the argument and the return type
unwrapFunMaybe :: Type -> Maybe (Type, Type)
unwrapFunMaybe (TypeApp (TypeApp f t1 _) t2 _)
    | f == typeFun = Just (t1, t2)
    | otherwise = Nothing
unwrapFunMaybe _ = Nothing
unwrapFun :: MonadError Text m => Type -> m (Type, Type)
unwrapFun t = maybe (throwError $ showt t <> " isn't a function type.") return (unwrapFunMaybe t)
unwrapSynFun :: MonadError Text m => HsType -> m (HsType, HsType)
unwrapSynFun (HsTyFun t1 t2) = return (t1, t2)
unwrapSynFun t               = throwError $ synPrint t <> " isn't a function type."

-- |Built-in types
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type
typeUnit = TypeCon $ TypeConstant "()" KindStar
typeBool = TypeCon $ TypeConstant "Bool" KindStar
typeInt = TypeCon $ TypeConstant "Int" KindStar
typeInteger = TypeCon $ TypeConstant "Integer" KindStar
typeFloat = TypeCon $ TypeConstant "Float" KindStar
typeDouble = TypeCon $ TypeConstant "Double" KindStar
typeChar = TypeCon $ TypeConstant "Char" KindStar

typeList, typeFun :: Type
typeList = TypeCon $ TypeConstant "[]" (KindFun KindStar KindStar)
typeFun = TypeCon $ TypeConstant "->" (KindFun KindStar $ KindFun KindStar KindStar)

typeTuple :: Int -> Type
typeTuple n = TypeCon $ TypeConstant sym (foldr KindFun KindStar $ replicate n KindStar)
    where sym = TypeVariableName $ makeTupleName n

typeString :: Type
typeString = makeListUnsafe typeChar

-- Utility functions for converting from our type representations to the AST representations and back
typeToSyn :: MonadError Text m => Type -> m Syntax.HsType
typeToSyn (TypeVar (TypeVariable (TypeVariableName name) _)) = return $ HsTyVar $ HsIdent $ unpack name
typeToSyn (TypeCon (TypeConstant (TypeVariableName name) _)) = return $ case name of
    "[]"  -> HsTyCon $ Special HsListCon
    ":"   -> HsTyCon $ Special HsCons
    "()"  -> HsTyCon $ Special HsUnitCon
    "->"  -> HsTyCon $ Special HsFunCon
    "(,)" -> HsTyCon $ Special $ HsTupleCon 0 -- TODO(kc506): Count commas, output specific type?
    _     -> HsTyCon $ UnQual $ HsIdent $ unpack name
typeToSyn (TypeApp t1 t2 _) = do
    t1' <- typeToSyn t1
    t2' <- typeToSyn t2
    return $ case t1' of
        HsTyApp (HsTyCon (Special HsFunCon)) t3 -> HsTyFun t3 t2'
        HsTyTuple ts                            -> HsTyTuple (ts ++ [t2'])
        HsTyCon (Special (HsTupleCon _))        -> HsTyTuple [t2']
        _                                       -> HsTyApp t1' t2'
synToType :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsType -> m Type
synToType ks t = evalStateT (synToType' t) ks

-- Helper version adds kinds as it processes the type: if we see `Functor a` then we remember `a :: * -> *`
synToType' :: (MonadState (M.Map TypeVariableName Kind) m, MonadError Text m) => Syntax.HsType -> m Type
synToType' (HsTyVar v) = TypeVar . TypeVariable name <$> gets (M.findWithDefault KindStar name)
    where name = convertName v
synToType' (HsTyCon c) = gets (M.lookup (TypeVariableName name)) >>= \case
    Nothing -> throwError $ "Type constructor not in kind mapping: " <> showt name
    Just k  -> return $ TypeCon $ TypeConstant (TypeVariableName name) k
    where name = convertName c
synToType' (HsTyFun arg body) = do
    arg' <- synToType' arg
    body' <- synToType' body
    makeFun [arg'] body'
synToType' (HsTyTuple ts) = makeTuple =<< mapM synToType' ts
synToType' t@HsTyApp{} = do
    let (base, args) = unmakeSynApp t
    base' <- synToType' base
    -- If the base type's kind is * then we don't know enough about it, so just assume it has the right number of
    -- arguments and they're all *. `minKind` is this simplest possible kind.
    let minKind = foldr KindFun KindStar $ replicate (length args) KindStar
        (base'', extraKinds) = case base' of
            TypeVar (TypeVariable v KindStar) -> (TypeVar $ TypeVariable v minKind, M.singleton v minKind)
            TypeCon (TypeConstant c KindStar) -> (TypeCon $ TypeConstant c minKind, M.singleton c minKind)
            _                                 -> (base', M.empty)
    (argKinds, _) <- unmakeKindFun $ kind base''
    let setKind (HsTyVar v) k = M.singleton (convertName v) k
        setKind _ _           = M.empty
        newKinds = M.unions $ extraKinds:zipWith setKind args argKinds
    modify (M.union newKinds)
    args' <- mapM synToType' args
    foldlM applyTypeFun base'' args'

typePredToSyn :: MonadError Text m => TypePredicate -> m Syntax.HsAsst
typePredToSyn (IsInstance (TypeVariableName c) t) = do
    t' <- typeToSyn t
    return (UnQual $ HsIdent $ unpack c, [t'])
synToTypePred :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsAsst -> m TypePredicate
synToTypePred ks (c, [t]) = IsInstance (convertName c) <$> synToType ks t
synToTypePred _ _         = throwError "Invalid constraint (unary or multiparameter)."

qualTypeToSyn :: MonadError Text m => QualifiedType -> m Syntax.HsQualType
qualTypeToSyn (Qualified quals t) = HsQualType <$> mapM typePredToSyn (S.toAscList quals) <*> typeToSyn t
synToQualType :: MonadError Text m => M.Map TypeVariableName Kind -> Syntax.HsQualType -> m QualifiedType
synToQualType ks (HsQualType quals t) = do
    quals' <- mapM (synToTypePred ks) quals
    -- Using the type constraints, infer the constraints of some of the type variables
    let makeKind (IsInstance c (TypeVar (TypeVariable v _))) = case M.lookup c ks of
            Nothing                  -> throwError $ "Missing kind for class " <> showt c
            Just (KindFun argKind _) -> return $ M.singleton v argKind
            Just k                   -> throwError $ "Invalid kind for class " <> showt c <> ": " <> showt k
        makeKind _ = return M.empty -- This should possibly handle adding kinds for constraints like `Functor [a]`?
    newKinds <- M.unions <$> mapM makeKind quals'
    Qualified (S.fromList quals') <$> synToType (M.union newKinds ks) t

class HasTypeVars t where
    getTypeVars :: t -> S.Set TypeVariable

instance HasTypeVars Type where
    getTypeVars (TypeVar v)       = S.singleton v
    getTypeVars (TypeCon _)       = S.empty
    getTypeVars (TypeApp t1 t2 _) = S.union (getTypeVars t1) (getTypeVars t2)
instance HasTypeVars TypePredicate where
    getTypeVars (IsInstance _ t) = getTypeVars t
instance HasTypeVars a => HasTypeVars (Qualified a) where
    getTypeVars (Qualified ps x) = getTypeVars ps `S.union` getTypeVars x
instance (Ord t, HasTypeVars t) => HasTypeVars (S.Set t) where
    getTypeVars = getTypeVars . S.toList
instance (Ord t, HasTypeVars t) => HasTypeVars (M.Map a t) where
    getTypeVars = getTypeVars . M.elems
instance HasTypeVars t => HasTypeVars [t] where
    getTypeVars = S.unions . map getTypeVars
instance HasTypeVars a => HasTypeVars (Maybe a) where
    getTypeVars = maybe S.empty getTypeVars
