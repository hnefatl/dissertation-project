module Backend.CodeGen.Hooks where

import BasicPrelude
import Backend.CodeGen.Converter
import           ExtraDefs                   (toLazyBytestring)
import Names (VariableName)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import           JVM.Builder                 hiding (locals)
import           JVM.ClassFile               hiding (Class, Field, Method, toString)
import Java.Lang (runtimeException, stringClass)

-- A collection of extra generator actions, used to generate extra code in the main class. Allows injecting
-- compiler-defined code like `error`, `undefined`, and dictionaries for builtin instances like `Num Int`.

compilerGeneratedHooks :: M.Map (S.Set VariableName) (Text -> Converter ())
compilerGeneratedHooks = M.fromList
    [ (S.fromList ["_compilerErrorImpl", "_makeCompilerError", "_compilerError"], \cname -> do
        -- Create a function to force a crash
        void $ newMethod [ACC_PUBLIC, ACC_STATIC] "_compilerErrorImpl" [] ReturnsVoid $ do
            -- Create a new exception and throw it
            new runtimeException
            dup
            ldc1 (CUTF8 "Compiler error :(") -- Load the error message from the constant pool
            invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
            throw
        -- Make a function to create a Function object of the crasher
        -- TODO(kc506): If we switch to makeFunction1, makeFunction2 etc, replace this
        makeImpl <- compileMakerFunction "_makeCompilerError" 1 0 "_compilerErrorImpl"
        -- Create a field we can treat as a variable in the program to call the crasher
        void $ makePublicStaticField "_compilerError" heapObjectClass $ \field -> do
            -- Store the Function object wrapping the implementation function in the field
            invokeStatic (toLazyBytestring cname) makeImpl
            putStaticField (toLazyBytestring cname) field
      )
    ]
--
--compilerGeneratedDatatypes :: MonadNameGenerator m => [m Datatype]
--compilerGeneratedDatatypes =
--    [
--        do
--            Datatype "(,)" []
--    ]