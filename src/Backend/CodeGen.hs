{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Backend.CodeGen where

import BasicPrelude
import TextShow (TextShow, showb, showt)
import System.FilePath ((</>), (<.>))
import qualified Data.ByteString.Lazy as B
import Data.Text (unpack)
import Control.Monad.State (MonadState, StateT, evalStateT)

import JVM.Assembler
import JVM.Builder
import JVM.Converter
import qualified JVM.ClassFile as ClassFile
import JVM.Exceptions
import qualified Java.Lang

import Backend.ILB hiding (ConverterState, Converter)

type Class = ClassFile.Class ClassFile.Direct
type Method = ClassFile.Method ClassFile.Direct

data NamedClass = NamedClass Text Class
    deriving (Eq, Show)
instance TextShow NamedClass where
    showb = fromString . show

writeClass :: FilePath -> NamedClass -> IO ()
writeClass directory (NamedClass name c) = B.writeFile (directory </> unpack name <.> "class") (encodeClass c)

data ConverterState = ConverterState { }
    deriving (Eq, Show)
instance TextShow ConverterState where
    showb = fromString . show

newtype Converter a = Converter (StateT ConverterState GeneratorIO a)
    deriving (Functor, Applicative, Monad, MonadState ConverterState, MonadGenerator)

--evalConverter :: Converter a -> ConverterState -> IO a
--evalConverter (Converter x) = generateT $ evalStateT x

--convertFunction :: Rhs -> Converter Method
--convertFunction (RhsClosure args body) = do
