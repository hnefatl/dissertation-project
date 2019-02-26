module Backend.CodeGen.Hooks where

import           Backend.CodeGen.Converter
import           Backend.CodeGen.JVMSanitisable (jvmSanitise)
import           BasicPrelude              hiding (ByteString)
import           Data.Text                 (unpack)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.Map.Strict           as M
import qualified Data.Set                  as S
import           ExtraDefs                 (toLazyByteString, fromLazyByteString)
import           Names                     (VariableName(..), TypeVariableName, convertName)

import           Java.Lang                 (runtimeException, stringClass)
import           JVM.Builder               hiding (locals)
import           JVM.Assembler
import           JVM.ClassFile             hiding (Class, Field, Method, toString)


-- |Primitive (nonboxed) datatypes. These are partially defined in the syntax (eg. `data Int`) but shouldn't be compiled
-- like other datatypes when it comes to code generation: we should rely on the runtime classes.
primitiveTypes :: S.Set TypeVariableName
primitiveTypes = S.fromList [ "_Int", "_Integer" ]

makeSimpleHook :: M.Map VariableName VariableName -> VariableName -> Int -> Converter () -> (S.Set VariableName, Text -> Converter ())
makeSimpleHook renamings symbol arity implementation = 
    (
        S.fromList [renameVar $ symbol <> "Impl", renameVar $ "make" <> symbol, renameVar symbol]
    ,
        \cname -> do
            -- Create an "Impl" function that perform the action
            void $ newMethod [ACC_PUBLIC, ACC_STATIC] (renameBs $ symbol <> "Impl") args ret implementation
            -- Make a function to create a Function object of the implementation function
            makeImpl <- compileMakerFunction (rename $ "make" <> symbol) arity 0 (rename $ symbol <> "Impl")
            -- Create a field we can treat as a variable in the program to call the crasher
            void $ makePublicStaticField (rename symbol) heapObjectClass $ \field -> do
                -- Store the Function object wrapping the implementation function in the field
                invokeStatic (toLazyByteString cname) makeImpl
                putStaticField (toLazyByteString cname) field
    )
    where
        rename n = jvmSanitise $ convertName $ M.findWithDefault n n renamings :: Text
        renameVar = VariableName . rename
        renameBs = toLazyByteString . rename
        args = [arrayOf heapObjectClass, arrayOf heapObjectClass]
        ret = Returns heapObjectClass

-- |A collection of extra generator actions, used to generate extra code in the main class. Allows injecting
-- compiler-defined code like `error`, `undefined`, and dictionaries for builtin instances like `Num Int`.
compilerGeneratedHooks :: M.Map VariableName VariableName -> M.Map (S.Set VariableName) (Text -> Converter ())
compilerGeneratedHooks renamings = M.fromList
    [ makeSimpleHook renamings "compilerError" 0 (throwRuntimeException "Compiler error :(")
    , makeSimpleHook renamings "undefined" 0 (throwRuntimeException "undefined")
    , makeSimpleHook renamings "primNumIntAdd" 2 $ invokeClassStaticMethod int "add" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntSub" 2 $ invokeClassStaticMethod int "sub" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntMult" 2 $ invokeClassStaticMethod int "mult" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntDiv" 2 $ invokeClassStaticMethod int "div" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primEqIntEq" 2 $ do
        -- Load and evaluate the arguments to Ints
        aload_ I0
        iconst_0
        aaload
        invokeVirtual heapObject enter
        checkCast int
        aload_ I0
        iconst_1
        aaload
        invokeVirtual heapObject enter
        checkCast int
        invokeStatic int $ NameType "eq" $ MethodSignature [intClass, intClass] (Returns $ ObjectType "java/lang/Boolean")
        -- Convert the Java Boolean to a Haskell Bool
        makeBoxedBool
        i0 ARETURN
    ]

invokeClassStaticMethod :: ByteString -> ByteString -> [ByteString] -> ReturnSignature -> Converter ()
invokeClassStaticMethod clsName methName args ret = do
    forM_ (zip args [0..]) $ \(arg, i) -> do
        -- Load the ith argument
        aload_ I0
        pushInt i
        aaload
        invokeVirtual heapObject enter
        checkCast arg
    invokeStatic clsName $ NameType methName $ MethodSignature (map (ObjectType . unpack . fromLazyByteString) args) ret
    i0 ARETURN

throwRuntimeException :: Text -> Converter ()
throwRuntimeException s = do
    -- Create a new exception and throw it
    new runtimeException
    dup
    loadString $ unpack s
    invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
    throw