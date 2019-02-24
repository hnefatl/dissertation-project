module Backend.CodeGen.Hooks where

import           Backend.CodeGen.Converter
import           Backend.CodeGen.JVMSanitisable (jvmSanitise)
import           BasicPrelude
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           ExtraDefs                 (toLazyBytestring)
import           Names                     (VariableName(..), TypeVariableName, convertName)

import           Java.Lang                 (runtimeException, stringClass)
import           JVM.Builder               hiding (locals)
import           JVM.ClassFile             hiding (Class, Field, Method, toString)


-- |Primitive (nonboxed) datatypes. These are partially defined in the syntax (eg. `data Int`) but shouldn't be compiled
-- like other datatypes when it comes to code generation: we should rely on the runtime classes.
primitiveTypes :: S.Set TypeVariableName
primitiveTypes = S.fromList [ "_Int", "_Integer" ]

-- |A collection of extra generator actions, used to generate extra code in the main class. Allows injecting
-- compiler-defined code like `error`, `undefined`, and dictionaries for builtin instances like `Num Int`.
compilerGeneratedHooks :: M.Map VariableName VariableName -> M.Map (S.Set VariableName) (Text -> Converter ())
compilerGeneratedHooks renamings = M.fromList
    [
        (
            S.fromList [renameVar "compilerErrorImpl", renameVar "makeCompilerError", renameVar "compilerError"]
        ,
            \cname -> do
                -- Create a function to force a crash
                void $ newMethod [ACC_PUBLIC, ACC_STATIC] (renameBs "compilerErrorImpl") args ret $ do
                    -- Create a new exception and throw it
                    new runtimeException
                    dup
                    loadString "Compiler error :("
                    invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
                    throw
                -- Make a function to create a Function object of the crasher
                -- TODO(kc506): If we switch to makeFunction1, makeFunction2 etc, replace this
                makeImpl <- compileMakerFunction (rename "makeCompilerError") 0 0 (rename "compilerErrorImpl")
                -- Create a field we can treat as a variable in the program to call the crasher
                void $ makePublicStaticField (rename "compilerError") heapObjectClass $ \field -> do
                    -- Store the Function object wrapping the implementation function in the field
                    invokeStatic (toLazyBytestring cname) makeImpl
                    putStaticField (toLazyBytestring cname) field
        )
    , 
        (
            S.singleton (renameVar "undefined")
        ,
            \cname -> do
                void $ newMethod [ACC_PUBLIC, ACC_STATIC] (renameBs "undefinedImpl") args ret $ do
                    new runtimeException
                    dup
                    loadString "undefined"
                    invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
                    throw
                makeImpl <- compileMakerFunction (rename "makeUndefined") 0 0 (rename "undefinedImpl")
                -- Create a field we can treat as a variable in the program to call the crasher
                void $ makePublicStaticField (rename "undefined") heapObjectClass $ \field -> do
                    -- Store the Function object wrapping the implementation function in the field
                    invokeStatic (toLazyBytestring cname) makeImpl
                    putStaticField (toLazyBytestring cname) field
        )
    ]
    where args = [arrayOf heapObjectClass, arrayOf heapObjectClass]
          ret = Returns heapObjectClass
          rename n = jvmSanitise $ convertName $ M.findWithDefault n n renamings :: Text
          renameVar = VariableName . rename
          renameBs = toLazyBytestring . rename