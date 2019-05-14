{-# LANGUAGE FlexibleInstances #-}

module Backend.CodeGen.JVMSanitisable where

import           BasicPrelude

import           Data.Bifunctor  (bimap)
import           Data.Char       (isAlphaNum, ord)
import qualified Data.Map.Strict as M
import           Data.Text       (pack, unpack)
import           Numeric         (showHex)

import           Backend.ILA     (Alt(..), AltConstructor(..), Binding(..), Datatype(..))
import           Backend.ILB     (Arg(..), Exp(..), Rhs(..))
import           Names           (TypeVariableName(..), VariableName(..))

-- Things that contain `VariableName`s that could contain invalid Java identifier characters like "<" can provide an
-- instance to rename them into valid identifiers like "3c".
class JVMSanitisable a where
    jvmSanitise :: a -> a
jvmSanitises :: (Functor t, JVMSanitisable a) => t a -> t a
jvmSanitises = fmap jvmSanitise

-- The only valid characters are unicode alphanumeric characters, the dollar sign, and the underscore. This is the set
-- of Java valid identifiers, which is a subset of the JVM valid identifiers. We write some bootstrap classes in Java
-- though, so we're limited to the Java characters.
-- We add the underscore to prevent the sanitised names from clashing with anything compiler-inserted like `HeapObject`.
instance JVMSanitisable String where
    jvmSanitise = ("_" <>) . concatMap (\c -> if isAlphaNum c then [c] else "$" <> showHex (ord c) "" <> "$")
instance JVMSanitisable Text where
    jvmSanitise = pack . jvmSanitise . unpack
instance JVMSanitisable TypeVariableName where
    jvmSanitise (TypeVariableName n) = TypeVariableName (jvmSanitise n)
instance JVMSanitisable VariableName where
    jvmSanitise (VariableName n) = VariableName (jvmSanitise n)
instance JVMSanitisable Arg where
    jvmSanitise l@ArgLit{} = l
    jvmSanitise (ArgVar v) = ArgVar (jvmSanitise v)
instance JVMSanitisable Exp where
    jvmSanitise l@ExpLit{}       = l
    jvmSanitise (ExpVar v)       = ExpVar (jvmSanitise v)
    jvmSanitise (ExpApp v as)    = ExpApp (jvmSanitise v) (jvmSanitises as)
    jvmSanitise (ExpConApp c as) = ExpConApp (jvmSanitise c) (jvmSanitises as)
    jvmSanitise (ExpCase s t as) = ExpCase (jvmSanitise s) t (jvmSanitises as)
    jvmSanitise (ExpLet v r e)   = ExpLet (jvmSanitise v) (jvmSanitise r) (jvmSanitise e)
instance JVMSanitisable Rhs where
    jvmSanitise (RhsClosure vs e) = RhsClosure (jvmSanitises vs) (jvmSanitise e)
instance JVMSanitisable a => JVMSanitisable (Alt a) where
    jvmSanitise (Alt c e) = Alt (jvmSanitise c) (jvmSanitise e)
instance JVMSanitisable AltConstructor where
    jvmSanitise (DataCon v vs) = DataCon (jvmSanitise v) (jvmSanitises vs)
    jvmSanitise Default        = Default
instance JVMSanitisable a => JVMSanitisable (Binding a) where
    jvmSanitise (NonRec v e) = NonRec (jvmSanitise v) (jvmSanitise e)
    jvmSanitise (Rec m)      = Rec $ M.fromList $ map (bimap jvmSanitise jvmSanitise) $ M.toList m

instance JVMSanitisable Datatype where
    jvmSanitise d = d
        { typeName = jvmSanitise $ typeName d
        , branches = M.mapKeys jvmSanitise (branches d) }
