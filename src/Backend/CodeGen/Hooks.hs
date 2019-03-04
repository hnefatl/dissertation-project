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
primitiveTypes = S.fromList [ "_Int", "_Integer", "_Char" ]

makeSimpleHook :: M.Map VariableName VariableName -> VariableName -> Int -> Converter () -> (S.Set VariableName, Text -> Converter ())
makeSimpleHook renamings symbol arity implementation = 
    (
        S.fromList [renameVar $ symbol <> "Impl", renameVar $ "make" <> symbol, renameVar symbol]
    ,
        \cname -> do
            -- Create an "Impl" function that perform the action
            void $ newMethod [ACC_PUBLIC, ACC_STATIC] (renameBs $ symbol <> "Impl") args ret $ do
                setLocalVarCounter 2 -- Account for the two local arrays
                implementation
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
    [ makeSimpleHook renamings "compilerError" 0 $ throwRuntimeException "Compiler error :("
    , makeSimpleHook renamings "undefined" 0 $ throwRuntimeException "undefined"
    , makeSimpleHook renamings "primNumIntAdd" 2 $ invokeClassStaticMethod int "add" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntSub" 2 $ invokeClassStaticMethod int "sub" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntMult" 2 $ invokeClassStaticMethod int "mult" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntDiv" 2 $ invokeClassStaticMethod int "div" [int, int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntNegate" 1 $ invokeClassStaticMethod int "negate" [int] (Returns intClass)
    , makeSimpleHook renamings "primNumIntFromInteger" 1 $ invokeClassStaticMethod int "fromInteger" [integer] (Returns intClass)
    , makeEq renamings "primEqIntEq" int
    , makeSimpleHook renamings "primOrdIntLess" 2 $ invokeClassStaticMethod int "less" [int, int] (Returns BoolType)
    , makeSimpleHook renamings "primNumIntegerAdd" 2 $ invokeClassStaticMethod integer "add" [integer, integer] (Returns integerClass)
    , makeSimpleHook renamings "primNumIntegerSub" 2 $ invokeClassStaticMethod integer "sub" [integer, integer] (Returns integerClass)
    , makeSimpleHook renamings "primNumIntegerMult" 2 $ invokeClassStaticMethod integer "mult" [integer, integer] (Returns integerClass)
    , makeSimpleHook renamings "primNumIntegerDiv" 2 $ invokeClassStaticMethod integer "div" [integer, integer] (Returns integerClass)
    , makeSimpleHook renamings "primNumIntegerNegate" 1 $ invokeClassStaticMethod integer "negate" [integer] (Returns integerClass)
    , makeEq renamings "primEqIntegerEq" integer
    , makeEq renamings "primEqCharEq" char
    , makeShow renamings "primShowIntShow" int
    , makeShow renamings "primShowIntegerShow" integer
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


makeEq :: M.Map VariableName VariableName -> VariableName -> ByteString -> (S.Set VariableName, Text -> Converter ())
makeEq renamings implName cls = makeSimpleHook renamings implName 2 $ do
    -- Load and evaluate the arguments to Ints
    forM_ [0, 1] $ \arg -> do
        loadLocal 0
        pushInt arg
        aaload
        invokeVirtual heapObject enter
        checkCast cls
    let cls' = ObjectType $ unpack $ fromLazyByteString cls
    invokeStatic cls $ NameType "eq" $ MethodSignature [cls', cls'] (Returns BoolType)
    -- Convert the Java Boolean to a Haskell Bool
    makeBoxedBool
    i0 ARETURN

makeShow :: M.Map VariableName VariableName -> VariableName -> ByteString -> (S.Set VariableName, Text -> Converter ())
makeShow renamings implName cls = makeSimpleHook renamings implName 1 $ do
    -- Load the argument and call `show` on it.
    loadLocal 0
    pushInt 0
    aaload
    let cls' = ObjectType $ unpack $ fromLazyByteString cls
    invokeStatic cls $ NameType "show" $ MethodSignature [cls'] (Returns Java.Lang.stringClass)
    makeBoxedString
    i0 ARETURN