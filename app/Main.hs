{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}

module Main where

import BasicPrelude
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Binary
import Text.Pretty.Simple

import JVM.Assembler
import JVM.Builder
import JVM.Converter
import JVM.ClassFile
import qualified Java.Lang
import qualified Java.IO

import Backend.CodeGen
import Data.Word ()

data BootstrapMethod = BootstrapMethod
    { bootstrapMethodRef :: Word16
    , bootstrapArguments :: [Word16] }
instance Binary BootstrapMethod where
    put m = do
        put $ bootstrapMethodRef m
        put $ (fromIntegral $ length $ bootstrapArguments m :: Word16)
        forM_ (bootstrapArguments m) put -- Don't use the default list instance as it prepends the length
    get = do
        ref <- get
        numArgs <- get :: Get Word16
        args <- replicateM (fromIntegral numArgs) get
        return $ BootstrapMethod { bootstrapMethodRef = ref, bootstrapArguments = args }

main :: IO ()
main = do
    let classname = "Test"
    ((ani, funIndex, arg1, arg2s, arg3), testClass) <- generateIO [] classname $ do
        -- Create the class
        makeTestClass
        -- Create a reference to the lambda creation method, for use in the bootstrap method
        let sigArgs = map ObjectType
                ["java/lang/invoke/MethodHandles$Lookup", "java/lang/String", "java/lang/invoke/MethodType", "java/lang/invoke/MethodType", "java/lang/invoke/MethodHandle", "java/lang/invoke/MethodType" ]
            metafactoryMethod = Method
                { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
                , methodName = "metafactory"
                , methodSignature = MethodSignature sigArgs (Returns $ ObjectType "java/lang/invoke/CallSite")
                , methodAttributesCount = 0
                , methodAttributes = AR M.empty
                }
        ani <- addToPool (CUTF8 "BootstrapMethods")
        lamMethHandle <- addToPool (CMethodHandle InvokeStatic "java/lang/invoke/LambdaMetaFactory" metafactoryMethod)
        arg1 <- addToPool (CMethodType "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
        arg3 <- addToPool (CMethodType "([LHeapObject;[LHeapObject;)LHeapObject;")
        let methods = [ "foo" ]
        arg2s <- forM methods $ \name -> do
            let method = Method
                    { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
                    , methodName = name
                    , methodSignature = MethodSignature [heapObjectClass, heapObjectClass] (Returns heapObjectClass)
                    , methodAttributesCount = 0
                    , methodAttributes = AR M.empty }
            addToPool (CMethodHandle InvokeStatic classname method)
        return (ani, lamMethHandle, arg1, arg2s, arg3)
    let classFile = classDirect2File testClass
        newAttributes =
            [ BootstrapMethod { bootstrapMethodRef = funIndex, bootstrapArguments = [ arg1, arg2, arg3 ] }
            | arg2 <- arg2s ]
        payload = encode (genericLength newAttributes :: Word16) <> mconcat (map encode newAttributes)
        bootstrapAttribute = Attribute
            { attributeName = ani
            , attributeLength = fromIntegral $ B.length payload
            , attributeValue = payload }
        classFile' = classFile
            { classAttributesCount = classAttributesCount classFile + 1
            , classAttributes = AP $ bootstrapAttribute:attributesList (classAttributes classFile) }
    pPrint classFile'
    putStrLn ""
    print funIndex
    B.writeFile "Test.class" $ encode classFile'

makeTestClass :: GeneratorIO ()
makeTestClass = do
    _ <- newMethod [ACC_PUBLIC, ACC_STATIC] "foo" [Java.Lang.objectClass] ReturnsVoid $ do
        i0 RETURN
    _ <- newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        new bifunction
        dup
        invokeDynamic 0 bifunctionApply
        --getStaticField Java.Lang.system Java.IO.out
        --invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN
    return ()
