{-# Language FlexibleContexts #-}
{-# Language Rank2Types #-}

module Main where

import BasicPrelude
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Pretty.Simple
import Data.Binary (encode)

import JVM.Assembler
import JVM.Builder
import JVM.Converter
import JVM.ClassFile
import Java.ClassPath
import qualified Java.Lang
import qualified Java.IO

import Backend.CodeGen
import Data.Word ()


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
        lamMethHandle <- addToPool (CMethodHandle InvokeStatic "java/lang/invoke/LambdaMetafactory" metafactoryMethod)
        arg1 <- addToPool (CMethodType "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
        arg3 <- addToPool (CMethodType "([LHeapObject;[LHeapObject;)LHeapObject;")
        let methods = [ "_fooImpl" ]
        arg2s <- forM methods $ \name -> do
            let method = Method
                    { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
                    , methodName = name
                    , methodSignature = MethodSignature [arrayOf heapObjectClass, arrayOf heapObjectClass] (Returns heapObjectClass)
                    , methodAttributesCount = 0
                    , methodAttributes = AR M.empty }
            addToPool (CMethodHandle InvokeStatic classname method)
        return (ani, lamMethHandle, arg1, arg2s, arg3)
    let classFile = classDirect2File testClass
        bootstrapMethods =
            [ BootstrapMethod { bootstrapMethodRef = funIndex, bootstrapArguments = [ arg1, arg2, arg3 ] }
            | arg2 <- arg2s ]
        bootstrapAttribute = toAttribute $ BootstrapMethodsAttribute
                { attributeNameIndex = ani
                , attributeMethods = bootstrapMethods }
        classFile' = classFile
            { classAttributesCount = classAttributesCount classFile + 1
            , classAttributes = AP $ bootstrapAttribute:attributesList (classAttributes classFile) }
    --pPrint classFile'
    --putStrLn ""
    B.writeFile "Test.class" $ encode classFile'

makeTestClass :: GeneratorIO ()
makeTestClass = do
    withClassPath $ addDirectory "/home/keith/project/compiler/javaexperiment/"
    let args = [arrayOf heapObjectClass, arrayOf heapObjectClass]
    -- foo = \x = y
    _ <- newMethod [ACC_PRIVATE, ACC_STATIC] "_fooImpl" args (Returns heapObjectClass) $ do
        -- Load the free variable array from first arg
        aload_ I1
        -- Fetch the 1st free variable
        iconst_0
        aaload
        -- Return it
        i0 ARETURN
    _ <- newMethod [ACC_PUBLIC, ACC_STATIC] "_makeFoo" [heapObjectClass] (Returns functionClass) $ do
        new function -- Create the function object we're going to return
        dup
        -- Create the bifunction
        invokeDynamic 0 bifunctionApply
        -- Arity
        iconst_1
        -- Free variables: create array of size 1 of HeapObjects
        iconst_1
        allocNewArray heapObject
        -- Store the free variable we were given as an argument
        dup
        iconst_0 -- Index 0
        aload_ I0 -- Argument 0
        aastore
        -- Call function constructor with the bifunction+arity+free variable array now on the stack
        invokeSpecial function functionInit
        i0 ARETURN
    _ <- newMethod [ACC_PUBLIC, ACC_STATIC] "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        -- Weirdly ordered: should generate code using local variables instead
        -- Create free variable (y)
        iconst_5
        invokeStatic "_Int" $ NameType "_makeInt" $ MethodSignature [IntType] (Returns $ ObjectType "_Int")
        -- Create function
        invokeStatic "Test" $ NameType "_makeFoo" $ MethodSignature [heapObjectClass] (Returns functionClass)
        dup
        -- Create argument (x)
        iconst_1
        invokeStatic "_Int" $ NameType "_makeInt" $ MethodSignature [IntType] (Returns $ ObjectType "_Int")
        -- Add argument
        invokeVirtual "Function" $ NameType "addArgument" $ MethodSignature [heapObjectClass] ReturnsVoid
        -- Enter
        invokeVirtual "Function" $ NameType "enter" $ MethodSignature [] (Returns heapObjectClass)
        invokeVirtual "HeapObject" $ NameType "toString" $ MethodSignature [] (Returns Java.Lang.stringClass)
        -- Print result (should be the free variable y), so an _Int with value 5
        getStaticField Java.Lang.system Java.IO.out
        i0 SWAP
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN
    return ()
