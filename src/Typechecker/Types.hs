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

-- |A type variable is a named variable tagged with a kind: eg. in the type expression `a b` we'd extract
-- `TypeVariable "a" (KindFun KindStar KindStar)` and `TypeVariable "b" KindStar`
data TypeVariable = TypeVariable !Id !Kind
-- |A dummy type variable: we can substitute on TypeVariables safely as their names should be "global", but a dummy type
-- variable has "local" names so isn't safe to substitute on (unrelated variables in different types may have the same
-- name).
data TypeDummy = TypeDummy !Id !Kind

-- |The type of a Haskell expression. Parameter allows for customisation of the value stored for a type variable
-- A `TypeConstant` is composed of the name of the constructor being used, along with 
data Type t = TypeVar !t
            | TypeConstant !Id ![Kind] ![Type t]
    deriving (Eq, Ord, Functor, Foldable, Traversable)

applyType :: (Show t, HasKind t, MonadError String m) => Type t -> Type t -> m (Type t)
applyType (TypeConstant name (k:ks) ts) t
    | k == getKind t = return $ TypeConstant name ks (t:ts)
    | otherwise = throwError $ printf "Got type of kind %s, expected kind %s" (show $ getKind t) (show k)
applyType (TypeConstant _ [] _) _ = throwError "Application of type of kind *"
-- TODO(kc506): This is valid (although maybe only with compiler extensions): `Monad m => a -> m a`
applyType t1 t2 = throwError $ printf "Can't apply type to a type variable - possible missing compiler feature: %s %s" (show t1) (show t2)

-- |"Unsafe" version of `applyType` which uses `error` instead of `throwError`: useful for compiler tests etc where
-- it's convenient to avoid boilerplate and we shouldn't have invalid types being used
applyTypeUnsafe :: (Show t, HasKind t) => Type t -> Type t -> Type t
applyTypeUnsafe t1 t2 = case runExcept $ applyType t1 t2 of
    Left err -> error err
    Right t -> t

-- |Type with sanitised variable names (safe to substitute on)
-- The need for these two types is subtle: say we're given constraints `Eq Int` and `Eq a => Eq [a]` and asked to check
-- if `[[Int]]` is an instance of `Eq`. If we initially unify `a` with `[Int]` then we ruin later substitutions as
-- we've polluted `a`. Instead, we call the constraint an uninstantiated constraint, and require it to be instantiated
-- such that the type variable names are unique across the typechecker before unifying. We'd then unify eg. `a0` with
--`[Int]`, `a1` with `Int`, and we're done.
type InstantiatedType = Type TypeVariable
-- |"Local" type that's not safe to substitute on
type UninstantiatedType = Type TypeDummy

-- |A type predicate, eg. `Ord a` becomes `IsInstance "Ord" (TypeVar (TypeVariable "a" KindStar))`
-- Used in quite a few places: as constraints on types and in class/instance declarations, eg.
-- `foo :: Ord a => a`, `class Eq a => Ord a`, `instance Eq Int`, `instance Eq a => Eq [a]`, ...
data TypePredicate t = IsInstance !Id !t deriving (Eq, Ord, Functor, Foldable, Traversable)
-- |Some common applications of type predicates
type InstantiatedTypePredicate = TypePredicate InstantiatedType
type UninstantiatedTypePredicate = TypePredicate UninstantiatedType

-- |A qualified thing: anywhere we can use `=>` is a qualified type, eg. `Eq a => Eq [a]` is a `Qualified
-- UninstantiatedType (TypePredicate UninstantiatedType)`, and `Eq a => a -> a -> a` is a `Qualified UninstantiatedType
-- UninstantiatedType`.
data Qualified t a = Qualified !(S.Set (TypePredicate t)) !a deriving (Eq, Ord)
-- |Some common applications of Qualified
type QualifiedType = Qualified InstantiatedType InstantiatedType
type UninstantiatedQualifiedType = Qualified UninstantiatedType UninstantiatedType
-- |A typeclass instance is eg. `instance Ord a => Ord [a]` or `instance Ord Int`.
type UninstantiatedClassInstance = Qualified UninstantiatedType UninstantiatedTypePredicate
type ClassInstance = Qualified InstantiatedType InstantiatedTypePredicate

instance Show TypeVariable where show (TypeVariable name _) = name
instance Show TypeDummy where show (TypeDummy name _) = name
instance Ord TypeVariable where compare (TypeVariable id1 _) (TypeVariable id2 _) = compare id1 id2
instance Ord TypeDummy where compare (TypeDummy id1 _) (TypeDummy id2 _) = compare id1 id2
instance Eq TypeVariable where TypeVariable id1 _ == TypeVariable id2 _ = id1 == id2
instance Eq TypeDummy where TypeDummy id1 _ == TypeDummy id2 _ = id1 == id2

instance (Show t, Show a) => Show (Qualified t a) where
    show (Qualified quals x) = qualifiers ++ " => " ++ show x
        where qualifiers = "(" ++ intercalate ", " (map show $ S.toList quals) ++ ")"


-- |A class for things that have a "kind": kinds themselves, various type variable/constant/dummies, and types.
class HasKind t where
    -- |Returns the kind of the given `t`
    getKind :: t -> Kind
instance HasKind Kind where
    getKind = id
instance HasKind TypeDummy where
    getKind (TypeDummy _ k) = k
instance HasKind TypeVariable where
    getKind (TypeVariable _ k) = k
instance HasKind a => HasKind (Type a) where
    getKind (TypeVar x) = getKind x
    -- The kind of an application is those kinds remaining in the application followed by a KindStar.
    getKind (TypeConstant _ kinds _) = foldr KindFun KindStar kinds

instance Show Kind where
    show = assocShow False
        where
        assocShow _ KindStar = "*"
        assocShow False (KindFun k1 k2) = assocShow True k1 ++ " -> " ++ assocShow False k2
        assocShow True k = "(" ++ assocShow False k ++ ")"
instance Show a => Show (Type a) where
    show (TypeVar x) = show x

    -- Special show cases for builtin types
    show (TypeConstant "[]" _ []) = "[]"
    show (TypeConstant "[]" _ [t]) = printf "[%s]" (show t)
    show (TypeConstant "[]" _ _) = error "compiler Error: Invalid type"

    show (TypeConstant "(,)" ks ts) = printf "(%s)" elements
        where elements = intercalate "," (map show (reverse ts) ++ replicate (length ks) "")

    show (TypeConstant "->" _ ts) = case ts of
            [] -> "(->)"
            [t] -> printf "(%s ->)" (assocShow t)
            [arg, ret] -> printf "%s -> %s" (assocShow arg) (show ret)
            _ -> error "Compiler Error: Invalid type"
        where assocShow (TypeConstant "->" _ ts') = if length ts' >= 2 then printf "(%s)" (show ts') else show ts'
              assocShow t = show t

    -- General show case
    show (TypeConstant name _ []) = name
    show (TypeConstant name _ ts) = printf "(%s %s)" name (unwords $ map show $ reverse ts)

instance Show t => Show (TypePredicate t) where
    show (IsInstance name t) = name ++ " (" ++ show t ++ ")"



-- |A monad that can convert a type with dummy variables into one with (uniquely) named variables
class Monad m => TypeInstantiator m where
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
    doInstantiate :: Instantiable a b => a -> m b
    doInstantiate x = evalStateT (instantiate freshNameMap x) M.empty

-- |Class of things that can be instantiated (from dummy variables to global variables) from a to b
-- Functional dependency `a -> b` enforces that each instance can map an `a` to exactly one `b`, removing any ambiguity.
class Instantiable a b | a -> b where
    -- |Given an action mapping local variable names to global ones, replace locals with globals
    instantiate :: Monad m => (Id -> m Id) -> a -> m b
    -- |"Cast" an instantiated expression into an uninstantiated one
    uninstantiate :: b -> a

instance Instantiable UninstantiatedType InstantiatedType where
    instantiate f = mapM (\(TypeDummy name kind) -> TypeVariable <$> f name <*> pure kind)
    uninstantiate = fmap (\(TypeVariable name kind) -> TypeDummy name kind)
instance Instantiable (TypePredicate UninstantiatedType) (TypePredicate InstantiatedType) where
    instantiate f (IsInstance super t) = IsInstance super <$> instantiate f t
    uninstantiate (IsInstance super t) = IsInstance super (uninstantiate t)
instance Instantiable UninstantiatedQualifiedType QualifiedType where
    instantiate f (Qualified quals t) = Qualified <$> instantiate f quals <*> instantiate f t
    uninstantiate (Qualified quals t) = Qualified (uninstantiate quals) (uninstantiate t)
instance (Ord a, Ord b, Instantiable a b) => Instantiable (S.Set a) (S.Set b) where
    instantiate f s = S.fromList <$> mapM (instantiate f) (S.toList s)
    uninstantiate = S.map uninstantiate
instance Instantiable a b => Instantiable [a] [b] where
    instantiate f = mapM (instantiate f)
    uninstantiate = map uninstantiate
instance Instantiable UninstantiatedClassInstance ClassInstance where
    instantiate f (Qualified quals t) = Qualified <$> instantiate f quals <*> instantiate f t
    uninstantiate (Qualified quals t) = Qualified (uninstantiate quals) (uninstantiate t)
instance Instantiable a b => Instantiable (Maybe a) (Maybe b) where
    instantiate f = mapM (instantiate f)
    uninstantiate = fmap uninstantiate


-- TODO(kc506): Find a better place to put these
-- |Utility functions on types
makeList :: (Show a, HasKind a) => Type a -> Type a
makeList = applyTypeUnsafe typeList
makeFun :: (Show a, HasKind a) => [Type a] -> Type a -> Type a
makeFun args ret = foldr (applyTypeUnsafe . applyTypeUnsafe typeFun) ret args
makeTuple :: (Show a, HasKind a) => [Type a] -> Type a
makeTuple elements = foldl' applyTypeUnsafe (typeTuple $ length elements) elements

-- |Built-in types
typeUnit, typeBool, typeInt, typeInteger, typeFloat, typeDouble, typeChar :: Type a
typeUnit = TypeConstant "()" [] []
typeBool = TypeConstant "Bool" [] []
typeInt = TypeConstant "Int" [] []
typeInteger = TypeConstant "Integer" [] []
typeFloat = TypeConstant "Float" [] []
typeDouble = TypeConstant "Double" [] []
typeChar = TypeConstant "Char" [] []

typeList, typeFun :: Type a
typeList = TypeConstant "[]" [KindStar] []
typeFun = TypeConstant "->" [KindStar, KindStar] []

typeTuple :: Int -> Type a
typeTuple n = TypeConstant "(,)" (replicate n KindStar) []

typeString :: (Show a, HasKind a) => Type a
typeString = makeList typeChar