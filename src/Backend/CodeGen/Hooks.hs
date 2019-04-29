module Backend.CodeGen.Hooks where

import           Backend.CodeGen.Converter
import           Backend.CodeGen.JVMSanitisable (jvmSanitise)
import           BasicPrelude                   hiding (ByteString)
import           Data.ByteString.Lazy           (ByteString)
import qualified Data.Map.Strict                as M
import qualified Data.Set                       as S
import           Data.Text                      (unpack)
import           ExtraDefs                      (fromLazyByteString, liftJoin2, toLazyByteString)
import           Names                          (TypeVariableName, VariableName(..), convertName)

import           Java.Lang                      (runtimeException, stringClass)
import           JVM.Assembler
import           JVM.Builder                    hiding (locals)
import           JVM.ClassFile                  hiding (Class, Field, Method, toString)


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
            heapObjectCls <- heapObjectClass
            let args = [arrayOf heapObjectCls, arrayOf heapObjectCls]
                ret = Returns heapObjectCls
            void $ newMethod [ACC_PUBLIC, ACC_STATIC] (renameBs $ symbol <> "Impl") args ret $ do
                setLocalVarCounter 2 -- Account for the two local arrays
                implementation
            -- Make a function to create a Function object of the implementation function
            makeImpl <- compileMakerFunction (rename $ "make" <> symbol) arity 0 (rename $ symbol <> "Impl")
            -- Create a field we can treat as a variable in the program to call the crasher
            void $ makePublicStaticField (rename symbol) heapObjectCls $ \field -> do
                -- Store the Function object wrapping the implementation function in the field
                invokeStatic (toLazyByteString cname) makeImpl
                putStaticField (toLazyByteString cname) field
    )
    where
        rename n = jvmSanitise $ convertName $ M.findWithDefault n n renamings :: Text
        renameVar = VariableName . rename
        renameBs = toLazyByteString . rename

-- |A collection of extra generator actions, used to generate extra code in the main class. Allows injecting
-- compiler-defined code like `error`, `undefined`, and dictionaries for builtin instances like `Num Int`.
compilerGeneratedHooks :: M.Map VariableName VariableName -> M.Map (S.Set VariableName) (Text -> Converter ())
compilerGeneratedHooks renamings = M.fromList
    [ makeSimpleHook renamings "compilerError" 0 $ throwRuntimeException "Compiler error :("
    , makeSimpleHook renamings "undefined" 0 $ throwRuntimeException "undefined"
    , makeSimpleHook renamings "error" 1 $ do
        -- Load the first argument, convert it to a Java string, throw it
        aload_ I0
        iconst_0
        aaload
        makeUnboxedString
        throwRuntimeExceptionFromStack
    , makeSimpleHook renamings "primNumIntAdd" 2 $ invokeClassStaticMethod int "add" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primNumIntSub" 2 $ invokeClassStaticMethod int "sub" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primNumIntMult" 2 $ invokeClassStaticMethod int "mult" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primNumIntDiv" 2 $ invokeClassStaticMethod int "div" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primNumIntNegate" 1 $ invokeClassStaticMethod int "negate" [int] (Returns <$> intClass)
    , makeSimpleHook renamings "primNumIntFromInteger" 1 $ invokeClassStaticMethod int "fromInteger" [integer] (Returns <$> intClass)
    , makeSimpleHook renamings "primEqIntEq" 2 $ invokeClassStaticMethodWith int "eq" [int, int] (pure $ Returns BoolType) makeBoxedBool
    , makeSimpleHook renamings "primOrdIntLess" 2 $ invokeClassStaticMethodWith int "less" [int, int] (pure $ Returns BoolType) makeBoxedBool
    , makeSimpleHook renamings "primNumIntegerAdd" 2 $ invokeClassStaticMethod integer "add" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primNumIntegerSub" 2 $ invokeClassStaticMethod integer "sub" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primNumIntegerMult" 2 $ invokeClassStaticMethod integer "mult" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primNumIntegerDiv" 2 $ invokeClassStaticMethod integer "div" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primNumIntegerNegate" 1 $ invokeClassStaticMethod integer "negate" [integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primIntegralIntDiv" 2 $ invokeClassStaticMethod int "div" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primIntegralIntMod" 2 $ invokeClassStaticMethod int "mod" [int, int] (Returns <$> intClass)
    , makeSimpleHook renamings "primIntegralIntegerDiv" 2 $ invokeClassStaticMethod integer "div" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primIntegralIntegerMod" 2 $ invokeClassStaticMethod integer "mod" [integer, integer] (Returns <$> integerClass)
    , makeSimpleHook renamings "primEqIntegerEq" 2 $ invokeClassStaticMethodWith integer "eq" [integer, integer] (pure $ Returns BoolType) makeBoxedBool
    , makeSimpleHook renamings "primEqCharEq" 2 $ invokeClassStaticMethodWith char "eq" [char, char] (pure $ Returns BoolType) makeBoxedBool
    , makeSimpleHook renamings "primShowIntShow" 1 $ invokeClassStaticMethodWith int "show" [int] (pure $ Returns Java.Lang.stringClass) makeBoxedString
    , makeSimpleHook renamings "primShowIntegerShow" 1 $ invokeClassStaticMethodWith integer "show" [integer] (pure $ Returns Java.Lang.stringClass) makeBoxedString
    ]

invokeClassStaticMethod :: Converter ByteString -> ByteString -> [Converter ByteString] -> Converter ReturnSignature -> Converter ()
invokeClassStaticMethod a b c d = invokeClassStaticMethodWith a b c d (return ())

invokeClassStaticMethodWith :: Converter ByteString -> ByteString -> [Converter ByteString] -> Converter ReturnSignature -> Converter () -> Converter ()
invokeClassStaticMethodWith clsName methName args ret action = do
    clsName' <- clsName
    args' <- sequence args
    ret' <- ret
    forM_ (zip args' [0..]) $ \(arg, i) -> do
        -- Load the ith argument
        aload_ I0
        pushInt i
        aaload
        liftJoin2 invokeVirtual heapObject enter
        checkCast arg
    invokeStatic clsName' $ NameType methName $ MethodSignature (map (ObjectType . unpack . fromLazyByteString) args') ret'
    action
    i0 ARETURN

throwRuntimeException :: Text -> Converter ()
throwRuntimeException s = do
    -- Create a new exception and throw it
    new runtimeException
    dup
    loadString $ unpack s
    invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
    throw

throwRuntimeExceptionFromStack :: Converter ()
throwRuntimeExceptionFromStack = do
    -- Create a new exception and throw it
    var <- storeUnnamedLocal
    new runtimeException
    dup
    loadLocal var
    invokeSpecial runtimeException $ NameType "<init>" $ MethodSignature [stringClass] ReturnsVoid
    throw
