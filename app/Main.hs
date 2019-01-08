{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}

module Main where

import BasicPrelude
import qualified Data.ByteString.Lazy as B

import JVM.Assembler
import JVM.Builder
import JVM.Converter
import JVM.ClassFile
import qualified Java.Lang
import qualified Java.IO

main :: IO ()
main = do
    testClass <- generateIO [] "Test" makeTestClass
    B.writeFile "Test.class" $ encodeClass testClass

makeTestClass :: GeneratorIO ()
makeTestClass = do
    foo <- newMethod [ACC_PUBLIC, ACC_STATIC] "foo" [] (Returns IntType) $ do
        getStaticField Java.Lang.system Java.IO.out
        loadString "Hello World"
        invokeVirtual Java.IO.printStream Java.IO.println
        iconst_1
        dup
        iadd
        i0 IRETURN
    _ <- newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        invokeStatic "Test" foo
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN
    return ()
