{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Backend.CodeGen.Converter where

import           BasicPrelude                hiding (encodeUtf8, head, init)
import           Control.Monad.Except        (Except, ExceptT, runExcept, throwError)
import           Control.Monad.State.Strict  (MonadState, StateT, get, gets, modify)
import qualified Data.ByteString.Lazy        as B
import           Data.Functor                (void)
import qualified Data.Map.Strict             as M
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import           Data.Word                   (Word16)
import           TextShow                    (TextShow, showb, showt)

import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                 hiding (locals)
import           JVM.ClassFile               hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile               as ClassFile

import           Backend.ILA                 (Literal(..), Datatype(..))
import           Backend.ILB
import           ExtraDefs                   (toLazyBytestring)
import           Logger                      (LoggerT, MonadLogger, writeLog)
import           NameGenerator
import           Names                       (TypeVariableName, VariableName(..), convertName)
import qualified Typechecker.Types           as Types

type Class = ClassFile.Class ClassFile.Direct
type OutputClass = ClassFile.Class ClassFile.File
type Method = ClassFile.Method ClassFile.Direct
type Field = ClassFile.Field ClassFile.Direct

type LocalVar = Word16

data NamedClass = NamedClass Text OutputClass
    deriving (Eq, Show)
instance TextShow NamedClass where
    showb = fromString . show

data ConverterState = ConverterState
    { -- The variable name that the entry point "_main" has been renamed to
      mainName :: VariableName
    ,  -- Which JVM local variable we're up to
      localVarCounter :: LocalVar
    , datatypes       :: M.Map TypeVariableName Datatype
    , -- Top-level variable names. Eg `x = 1 ; y = let z = 2 in z` has top-level variable names x and y. This is used
      -- when working out which free variables we need to pass to functions, and which it can get for itself.
      -- The values associated with each symbol is the type of the equivalent field in the class (usually HeapObject).
      topLevelSymbols :: M.Map VariableName FieldType
    , -- A reverse mapping from the renamed top-level variables to what they were originally called.
      reverseRenames :: M.Map VariableName VariableName
    , -- Local variable names, and an action that can be used to push them onto the stack
      localSymbols    :: M.Map VariableName (Converter ())
    , -- A list of initialisation actions to run as the first instructions inside `main`. Used for initialising fields.
      initialisers    :: [Converter ()]
    , -- Which methods we're using an invokeDynamic instruction on, so we need to add bootstrap methods for
      dynamicMethods  :: Seq.Seq Text
    , -- Class instances like `Num Int`. We want to compile any ground instances into static fields.
      dictionaries    :: S.Set Types.TypePredicate
    , -- The name of the class we're compiling to
      classname       :: B.ByteString }

-- A `Converter Method` action represents a computation that compiles a method: the ConverterState should be reset
-- inbetween method compilations
newtype Converter a = Converter (StateT ConverterState (GeneratorT (ExceptT Text (LoggerT (NameGeneratorT IO)))) a)
    deriving (Functor, Applicative, Monad, MonadState ConverterState, MonadGenerator, MonadLogger, MonadNameGenerator, MonadIO)

throwTextError :: Text -> Converter a
throwTextError = liftErrorText . throwError
liftErrorText :: Except Text a -> Converter a
liftErrorText x = case runExcept x of
    Left e  -> Converter $ lift $ lift $ throwError e
    Right y -> return y
liftExc :: ExceptT Text (LoggerT (NameGeneratorT IO)) a -> Converter a
liftExc = Converter . lift . lift

setLocalVarCounter :: Word16 -> Converter ()
setLocalVarCounter x = modify (\s -> s { localVarCounter = x })
getFreshLocalVar :: Converter Word16
getFreshLocalVar = do
    x <- gets localVarCounter
    when (x == maxBound) $ throwTextError "Maximum number of local variables met"
    modify (\s -> s { localVarCounter = x + 1 })
    return x

-- |Record that the given variable is in the given local variable index
addLocalVariable :: VariableName -> LocalVar -> Converter ()
addLocalVariable v i = do
    writeLog $ "Local variable " <> showt v <> " stored with index " <> showt i
    addComplexLocalVariable v (void $ loadLocal i)
-- |Same as addLocalVariable but with a user-defined action to push the variable onto the stack
addComplexLocalVariable :: VariableName -> Converter () -> Converter ()
addComplexLocalVariable v action = do
    writeLog $ "Local variable " <> showt v <> " stored with custom pusher"
    modify (\s -> s { localSymbols = M.insert v action (localSymbols s) } )
pushSymbol :: VariableName -> Converter ()
pushSymbol v = do
    globals <- gets topLevelSymbols
    locals <- gets localSymbols
    case (M.lookup v globals, M.member v locals) of
        (Nothing, False) -> throwTextError $ "Variable " <> showt v <> " not found in locals or globals"
        (Just _, True)   -> throwTextError $ "Variable " <> showt v <> " found in both locals and globals"
        (Just t, False)  -> pushGlobalSymbol v t
        (Nothing, True)  -> pushLocalSymbol v
pushGlobalSymbol :: VariableName -> FieldType -> Converter ()
pushGlobalSymbol v t = do
    writeLog $ "Pushing " <> showt v <> " from globals"
    cname <- gets classname
    renames <- gets reverseRenames
    getStaticField cname $ NameType { ntName = toLazyBytestring $ convertName v, ntSignature = t }
pushLocalSymbol :: VariableName -> Converter ()
pushLocalSymbol v = gets (M.lookup v . localSymbols) >>= \case
    Just pusher -> do
        writeLog $ "Pushing " <> showt v <> " from locals"
        pusher
    Nothing -> throwTextError $ "No action for local symbol " <> showt v

-- |Handles resetting some state after running the action
inScope :: Converter a -> Converter a
inScope action = do
    s <- get
    res <- action
    modify $ \s' -> s'
        { localVarCounter = localVarCounter s
        , localSymbols = localSymbols s }
    return res

addDynamicMethod :: Text -> Converter Word16
addDynamicMethod m = do
    ms <- gets dynamicMethods
    modify (\s -> s { dynamicMethods = dynamicMethods s Seq.|> m })
    return $ fromIntegral $ Seq.length ms

-- The support classes written in Java and used by the Haskell translation
heapObject, function, thunk, bifunction, unboxedData, boxedData, int :: IsString s => s
heapObject = "HeapObject"
function = "Function"
thunk = "Thunk"
bifunction = "java/util/function/BiFunction"
unboxedData = "Data"
boxedData = "BoxedData"
int = "_Int"
heapObjectClass, functionClass, thunkClass, bifunctionClass, unboxedDataClass, boxedDataClass, intClass :: FieldType
heapObjectClass = ObjectType heapObject
functionClass = ObjectType function
thunkClass = ObjectType thunk
bifunctionClass = ObjectType bifunction
unboxedDataClass = ObjectType unboxedData
boxedDataClass = ObjectType boxedData
intClass = ObjectType int
addArgument :: NameType Method
addArgument = ClassFile.NameType "addArgument" $ MethodSignature [heapObjectClass] ReturnsVoid
enter :: NameType Method
enter = NameType "enter" $ MethodSignature [] (Returns heapObjectClass)
force :: NameType Method
force = NameType "force" $ MethodSignature [] (Returns heapObjectClass)
functionInit :: NameType Method
functionInit = NameType "<init>" $ MethodSignature [bifunctionClass, IntType, arrayOf heapObjectClass] ReturnsVoid
thunkInit :: NameType Method
thunkInit = NameType "<init>" $ MethodSignature [heapObjectClass] ReturnsVoid
bifunctionApply :: NameType Method
bifunctionApply = NameType "apply" $ MethodSignature [] (Returns bifunctionClass)
clone :: NameType Method
clone = NameType "clone" $ MethodSignature [] (Returns Java.Lang.objectClass)
toString :: NameType Method
toString = NameType "toString" $ MethodSignature [] (Returns Java.Lang.stringClass)
makeInt :: NameType Method
makeInt = NameType "_makeInt" $ MethodSignature [IntType] (Returns intClass)
boxedDataBranch :: NameType Field
boxedDataBranch = NameType "branch" $ IntType
boxedDataData :: NameType Field
boxedDataData = NameType "data" $ arrayOf heapObjectClass

-- |Create a new public static field in this class with the given name and type.
-- The given action will be run at the start of `main`, and should be used to initialise the field
makePublicStaticField :: Text -> FieldType -> (ClassFile.NameType Field -> Converter ()) -> Converter (ClassFile.NameType Field)
makePublicStaticField name fieldType init = do
    field <- newField [ ACC_PUBLIC, ACC_STATIC ] (toLazyBytestring name) fieldType
    modify $ \s -> s
        { initialisers = initialisers s <> [init field]
        , topLevelSymbols = M.insert (VariableName name) fieldType (topLevelSymbols s) }
    return field

-- Create a function that wraps a Function object around an implementation (java) function
compileMakerFunction :: Text -> Int -> Int -> Text -> Converter (ClassFile.NameType Method)
compileMakerFunction name arity numFreeVars implName = do
    let accs = [ACC_PUBLIC, ACC_STATIC]
        wrapperArgs = replicate numFreeVars heapObjectClass
    inScope $ newMethod accs (toLazyBytestring name) wrapperArgs (Returns functionClass) $ do
        setLocalVarCounter (fromIntegral numFreeVars) -- Local variables [0..numFreeVars) are used for arguments
        -- Record that we're using a dynamic function invocation, remember the implementation function's name
        methodIndex <- addDynamicMethod implName
        -- This is the function we're going to return at the end
        new function
        dup
        -- Invoke the bootstrap method for this dynamic call site to create our BiFunction instance
        invokeDynamic methodIndex bifunctionApply
        -- Push the arity of the function
        pushInt arity
        -- Create an array of HeapObjects holding the free variables we were given as arguments
        pushInt numFreeVars
        allocNewArray heapObject
        forM_ [0..numFreeVars - 1] $ \fv -> do
            -- For each free variable argument, push it into the same index in the array
            dup
            pushInt fv
            loadLocal (fromIntegral fv)
            aastore
        -- Call the Function constructor with the bifunction, the arity, and the free variable array
        invokeSpecial function functionInit
        i0 ARETURN


pushArg :: Arg -> Converter ()
pushArg (ArgLit l) = pushLit l
pushArg (ArgVar v) = pushSymbol v

pushLit :: Literal -> Converter ()
pushLit (LiteralInt i)    = do
    pushInt $ fromIntegral i
    invokeStatic int makeInt
pushLit (LiteralChar _)   = throwTextError "Need support for characters"
pushLit (LiteralString _) = throwTextError "Need support for strings"
pushLit (LiteralFrac _)   = throwTextError "Need support for rationals"

pushInt :: MonadGenerator m => Int -> m ()
pushInt (-1) = iconst_m1
pushInt 0 = iconst_0
pushInt 1 = iconst_1
pushInt 2 = iconst_2
pushInt 3 = iconst_3
pushInt 4 = iconst_4
pushInt 5 = iconst_5
pushInt i
    -- TODO(kc506): Check sign's preserved. JVM instruction takes a signed Word8, haskell's is unsigned.
    | i >= -128 && i <= 127 = bipush $ fromIntegral i
    | i >= -32768 && i <= 32767 = sipush $ fromIntegral i
    | otherwise = error "Need support for: 32 bit integers and arbitrary-sized integers"

storeLocal :: VariableName -> Converter LocalVar
storeLocal v = do
    localVar <- getFreshLocalVar
    storeSpecificLocal v localVar
    return localVar

storeSpecificLocal :: VariableName -> LocalVar -> Converter ()
storeSpecificLocal v i = do
    case i of
        0 -> astore_ I0
        1 -> astore_ I1
        2 -> astore_ I2
        3 -> astore_ I3
        _ -> if i < 256 then astore $ fromIntegral i else astorew $ fromIntegral i
    addLocalVariable v i

loadLocal :: MonadGenerator m => Word16 -> m ()
loadLocal 0 = aload_ I0
loadLocal 1 = aload_ I1
loadLocal 2 = aload_ I2
loadLocal 3 = aload_ I3
loadLocal i = if i < 256 then aload $ fromIntegral i else aloadw $ fromIntegral i