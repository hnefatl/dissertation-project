{-# Language FlexibleContexts, LambdaCase, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies #-}
{-# Language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- We require undecidable instances to make Sets Instantiable
{-# Language UndecidableInstances#-}

module Typechecker.Types where

import Control.Monad.Except
import Control.Monad.State.Strict
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (intercalate, foldl')

-- |General variable/type name
type Id = String

-- |A kind is the "type of a type": either `*` or `Kind -> Kind`
-- Int has kind *, Maybe has kind * -> *, Either has kind * -> * -> *
data Kind = KindStar | KindFun !Kind !Kind deriving (Eq, Ord)

-- |The number of type arguments that a kind represents: this is one less than the number of nodes on the right-most
-- branch of a "Kind tree".
getKindArgCount :: Integral i => Kind -> i
getKindArgCount KindStar = 0
getKindArgCount (KindFun _ k) = 1 + getKindArgCount k

data TypeVariable = TypeVariable !Id !Kind deriving (Eq, Ord)

-- |The type of a Haskell expression.
-- A `TypeVariable` is a globally unique name for a type variable.
-- A `TypeDummy` is a locally (within the type) unique name for a type variable.
-- A `TypeConstant` is composed of the name of the constructor being used, along with the kinds of any remaining
-- arguments and the list of applied types
data Type = TypeVar !TypeVariable
          | TypeDummy !TypeVariable
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
data TypePredicate = IsInstance !Id !Type deriving (Eq, Ord)

-- |A qualified thing: anywhere we can use `=>` is a qualified type, eg. `Eq a => Eq [a]` is a `Qualified
-- UninstantiatedType (TypePredicate UninstantiatedType)`, and `Eq a => a -> a -> a` is a `Qualified UninstantiatedType
-- UninstantiatedType`.
data Qualified a = Qualified !(S.Set TypePredicate) !a deriving (Eq, Ord)
-- |Some common applications of Qualified
type QualifiedType = Qualified Type
-- |A typeclass instance is eg. `instance Ord a => Ord [a]` or `instance Ord Int`.
type ClassInstance = Qualified TypePredicate

instance Show a => Show (Qualified a) where
    show (Qualified quals x) = qualifiers ++ " => " ++ show x
        where qualifiers = "(" ++ intercalate ", " (map show $ S.toList quals) ++ ")"


-- |A class for things that have a "kind": kinds themselves, various type variable/constant/dummies, and types.
class HasKind t where
    -- |Returns the kind of the given `t`
    getKind :: t -> Kind
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = k
instance HasKind Type where
    getKind (TypeVar v) = getKind v
    getKind (TypeDummy v) = getKind v
    -- The kind of an application is those kinds remaining in the application followed by a KindStar.
    getKind (TypeConstant _ kinds _) = foldr KindFun KindStar kinds

instance Show Kind where
    show = assocShow False
        where
        assocShow _ KindStar = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 ++ " -> " ++ assocShow False k2
        assocShow True k = "(" ++ assocShow False k ++ ")"
instance Show TypeVariable where
    show (TypeVariable name _) = name
instance Show Type where
    show (TypeVar v) = show v
    show (TypeDummy v) = show v

    -- Special show cases for builtin types
    show (TypeConstant "[]" _ []) = "[]"
    show (TypeConstant "[]" _ [t]) = printf "[%s]" (show t)
    show (TypeConstant "[]" _ _) = error "compiler Error: Invalid type"

    show (TypeConstant "(,)" ks ts) = printf "(%s)" elements
        where elements = intercalate "," (map show ts ++ replicate (length ks) "")

    show (TypeConstant "->" _ ts) = case ts of
            [] -> "(->)"
            [t] -> printf "(%s ->)" (assocShow t)
            [arg, ret] -> printf "%s -> %s" (show arg) (assocShow ret)
            _ -> error "Compiler Error: Invalid type"
        where assocShow t@(TypeConstant "->" _ ts') = if length ts' >= 2 then printf "(%s)" (show t) else show t
              assocShow t = show t

    -- General show case
    show (TypeConstant name _ []) = name
    show (TypeConstant name _ ts) = printf "(%s %s)" name (unwords $ map show ts)

instance Show TypePredicate where
    show (IsInstance name t) = name ++ " (" ++ show t ++ ")"


-- |A monad that can convert a type with dummy variables into one with (uniquely) named variables
class MonadError String m => TypeInstantiator m where
    -- |Should generate a new unique name each time it's run
    freshName :: m Id

    -- |Utility state monad function: given a local name, convert it to a global name, ensuring that matching local
    -- names map to the same global name
    freshNameMap :: Id -> StateT (M.Map Id Id) m Id
    freshNameMap localName = gets (M.lookup localName) >>= \case
            Nothing -> do
                globalName <- lift freshName
                state $ \s -> (globalName, M.insert localName globalName s)
            Just globalName -> return globalName
            
    -- |Instantiate an instantiable, ensuring that variables of the same local name within the structure get mapped to
    -- the *same globally unique name*.
    doInstantiate :: Instantiable a => a -> m a
    doInstantiate x = evalStateT (instantiate freshNameMap x) M.empty

-- |Class of things that can be instantiated (from dummy variables to global variables) from a to b
-- Functional dependency `a -> b` enforces that each instance can map an `a` to exactly one `b`, removing any ambiguity.
class Instantiable a where
    -- |Given an action mapping local variable names to global ones, replace locals with globals
    instantiate :: MonadError String m => (Id -> m Id) -> a -> m a
    -- |"Cast" an instantiated expression into an uninstantiated one
    uninstantiate :: a -> a

instance Instantiable TypeVariable where
    instantiate f (TypeVariable name kind) = TypeVariable <$> f name <*> pure kind
    uninstantiate = id
instance Instantiable Type where
    instantiate f (TypeDummy v) = TypeVar <$> instantiate f v
    instantiate _ v@(TypeVar _) = throwError ("Probable compiler error when instantiating " ++ show v)
    instantiate f (TypeConstant name ks ts) = TypeConstant name ks <$> mapM (instantiate f) ts

    uninstantiate (TypeVar v) = TypeDummy (uninstantiate v)
    uninstantiate d@(TypeDummy _) = error ("Probable compiler error when uninstantiating " ++ show d)
    uninstantiate (TypeConstant name ks ts) = TypeConstant name ks (map uninstantiate ts)
instance Instantiable TypePredicate where
    instantiate f (IsInstance super t) = IsInstance super <$> instantiate f t
    uninstantiate (IsInstance super t) = IsInstance super (uninstantiate t)
instance Instantiable QualifiedType where
    instantiate f (Qualified quals t) = Qualified <$> instantiate f quals <*> instantiate f t
    uninstantiate (Qualified quals t) = Qualified (uninstantiate quals) (uninstantiate t)
instance (Ord a, Instantiable a) => Instantiable (S.Set a) where
    instantiate f s = S.fromList <$> mapM (instantiate f) (S.toList s)
    uninstantiate = S.map uninstantiate
instance Instantiable a => Instantiable [a] where
    instantiate f = mapM (instantiate f)
    uninstantiate = map uninstantiate
instance Instantiable ClassInstance where
    instantiate f (Qualified quals t) = Qualified <$> instantiate f quals <*> instantiate f t
    uninstantiate (Qualified quals t) = Qualified (uninstantiate quals) (uninstantiate t)
instance Instantiable a => Instantiable (Maybe a) where
    instantiate f = mapM (instantiate f)
    uninstantiate = fmap uninstantiate


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
typeUnit = TypeConstant "()" [] []
typeBool = TypeConstant "Bool" [] []
typeInt = TypeConstant "Int" [] []
typeInteger = TypeConstant "Integer" [] []
typeFloat = TypeConstant "Float" [] []
typeDouble = TypeConstant "Double" [] []
typeChar = TypeConstant "Char" [] []

typeList, typeFun :: Type
typeList = TypeConstant "[]" [KindStar] []
typeFun = TypeConstant "->" [KindStar, KindStar] []

typeTuple :: Int -> Type
typeTuple n = TypeConstant "(,)" (replicate n KindStar) []

typeString :: Type
typeString = makeList typeChar