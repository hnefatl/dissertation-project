{-# LANGUAGE FlexibleInstances          #-}

module Backend.CodeGen.JVMSanitisable where

import BasicPrelude

import           Numeric                     (showHex)
import           Data.Char                   (isAlphaNum, ord)
import           Data.Text                   (pack, unpack)
import           Data.Bifunctor              (bimap)
import qualified Data.Map.Strict as M

import Names (VariableName(..))
import Backend.ILA (Binding(..), Alt(..))
import Backend.ILB (Arg(..), Exp(..), Rhs(..))

-- Things that contain `VariableName`s that could contain invalid Java identifier characters like "<" can provide an
-- instance to rename them into valid identifiers like "3c".
class JVMSanitisable a where
    jvmSanitise :: a -> a
jvmSanitises :: JVMSanitisable a => [a] -> [a]
jvmSanitises = map jvmSanitise

-- The only valid characters are unicode alphanumeric characters, the dollar sign, and the underscore. This is the set
-- of Java valid identifiers, which is a subset of the JVM valid identifiers. We write some bootstrap classes in Java
-- though, so we're limited to the Java characters.
-- We add the underscore to prevent the sanitised names from clashing with anything compiler-inserted like `HeapObject`.
instance JVMSanitisable String where
    jvmSanitise = ("_" <>) . concatMap (\c -> if validChar c then [c] else showHex (ord c) "")
        where validChar c = isAlphaNum c || c == '$' || c == '_'
instance JVMSanitisable Text where
    jvmSanitise = pack . jvmSanitise . unpack
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
    jvmSanitise (ExpCase s bs as) = ExpCase (jvmSanitise s) (jvmSanitises bs) (jvmSanitises as)
    jvmSanitise (ExpLet v r e) = ExpLet (jvmSanitise v) (jvmSanitise r) (jvmSanitise e)
instance JVMSanitisable Rhs where
    jvmSanitise (RhsClosure vs e) = RhsClosure (jvmSanitises vs) (jvmSanitise e)
instance JVMSanitisable a => JVMSanitisable (Alt a) where
    jvmSanitise (Alt c vs e) = Alt c (jvmSanitises vs) (jvmSanitise e)
instance JVMSanitisable a => JVMSanitisable (Binding a) where
    jvmSanitise (NonRec v e) = NonRec (jvmSanitise v) (jvmSanitise e)
    jvmSanitise (Rec m) = Rec $ M.fromList $ map (bimap jvmSanitise jvmSanitise) $ M.toList m