{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Backend.CodeGen.Converter where

import           BasicPrelude                   hiding (bool, encodeUtf8, head, init, inits, swap)
import           Control.Monad.Except           (Except, ExceptT, runExcept, throwError)
import           Control.Monad.State.Strict     (MonadState, StateT, get, gets, modify)
import qualified Data.ByteString.Lazy           as B
import           Data.Functor                   (void, (<&>))
import qualified Data.Map.Strict                as M
import qualified Data.Sequence                  as Seq
import           Data.Text                      (unpack)
import           Data.Word                      (Word16)
import           TextShow                       (TextShow, showb, showt)

import qualified Java.IO
import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                    hiding (locals)
import           JVM.ClassFile                  hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile                  as ClassFile

import           Backend.CodeGen.JVMSanitisable (jvmSanitise)
import           Backend.ILA                    (Datatype(..), Literal(..))
import           Backend.ILB
import           ExtraDefs                      (fromLazyByteString, liftJoin2, toLazyByteString)
import           Logger                         (LoggerT, MonadLogger)
import           NameGenerator
import           Names                          (TypeVariableName, VariableName(..), convertName)

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
    { -- The variable name that the entry point "main" has been renamed to
      mainName        :: VariableName
    ,  -- The Java package to place the class files in
      packageName     :: String
    ,  -- Which JVM local variable we're up to
      localVarCounter :: LocalVar
    , datatypes       :: M.Map TypeVariableName Datatype
    , -- Top-level variable names. Eg `x = 1 ; y = let z = 2 in z` has top-level variable names x and y. This is used
      -- when working out which free variables we need to pass to functions, and which it can get for itself.
      -- The values associated with each symbol is the type of the equivalent field in the class (usually HeapObject).
      topLevelSymbols :: M.Map VariableName FieldType
    , -- A forward mapping from original top-level variables to their renamed versions
      topLevelRenames :: M.Map VariableName VariableName
    , -- A reverse mapping from the renamed top-level variables to what they were originally called.
      reverseRenames  :: M.Map VariableName VariableName
    , -- Local variable names, and an action that can be used to push them onto the stack
      localSymbols    :: M.Map VariableName (Converter ())
    , -- A list of initialisation actions to run as the first instructions inside `main`. Used for initialising fields.
      initialisers    :: [Converter ()]
    , -- Which methods we're using an invokeDynamic instruction on, so we need to add bootstrap methods for
      dynamicMethods  :: Seq.Seq Text
    , -- The name of the class we're compiling to
      classname       :: B.ByteString }

getPackageName :: IsString s => Converter s
getPackageName = gets (fromString . packageName)

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

setLocalVarCounter :: LocalVar -> Converter ()
setLocalVarCounter x = modify (\s -> s { localVarCounter = x })
getFreshLocalVar :: Converter LocalVar
getFreshLocalVar = do
    x <- gets localVarCounter
    when (x == maxBound) $ throwTextError "Maximum number of local variables met"
    modify (\s -> s { localVarCounter = x + 1 })
    return x

-- |Record that the given variable is in the given local variable index
addLocalVariable :: VariableName -> LocalVar -> Converter ()
addLocalVariable v i = do
    --writeLog $ "Local variable " <> showt v <> " stored with index " <> showt i
    addComplexLocalVariable v (void $ loadLocal i)
-- |Same as addLocalVariable but with a user-defined action to push the variable onto the stack
addComplexLocalVariable :: VariableName -> Converter () -> Converter ()
addComplexLocalVariable v action = do
    --writeLog $ "Local variable " <> showt v <> " stored with custom pusher"
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
    --writeLog $ "Pushing " <> showt v <> " from globals"
    cname <- gets classname
    getStaticField cname $ NameType { ntName = toLazyByteString $ convertName v, ntSignature = t }
pushLocalSymbol :: VariableName -> Converter ()
pushLocalSymbol v = gets (M.lookup v . localSymbols) >>= \case
    Just pusher -> do
        --writeLog $ "Pushing " <> showt v <> " from locals"
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

makeClass :: IsString s => String -> Converter s
makeClass s = fromString . (<> ("/" <> s)) <$> getPackageName
-- The support classes written in Java and used by the Haskell translation
heapObject, function, unboxedData, boxedData, int, integer, char, bool, list :: IsString s => Converter s
heapObject = makeClass "HeapObject"
function = makeClass "Function"
unboxedData = makeClass "Data"
boxedData = makeClass "BoxedData"
int = makeClass "_Int"
integer = makeClass "_Integer"
char = makeClass "_Char"
bool = makeClass "_Bool"
list = makeClass $ jvmSanitise "[]"
bifunction :: IsString s => s
bifunction = "java/util/function/BiFunction"
heapObjectClass, functionClass, unboxedDataClass, boxedDataClass, intClass, integerClass, charClass, boolClass, listClass :: Converter FieldType
heapObjectClass = ObjectType <$> heapObject
functionClass = ObjectType <$> function
unboxedDataClass = ObjectType <$> unboxedData
boxedDataClass = ObjectType <$> boxedData
intClass = ObjectType <$> int
integerClass = ObjectType <$> integer
charClass = ObjectType <$> char
boolClass = ObjectType <$> bool
listClass = ObjectType <$> list
bifunctionClass :: FieldType
bifunctionClass = ObjectType bifunction
addArgument :: Converter (NameType Method)
addArgument = heapObjectClass <&> \cls -> ClassFile.NameType "addArgument" $ MethodSignature [cls] ReturnsVoid
enter :: Converter (NameType Method)
enter = heapObjectClass <&> \cls -> NameType "enter" $ MethodSignature [] (Returns cls)
functionInit :: Converter (NameType Method)
functionInit = do
    hoCls <- heapObjectClass
    return $ NameType "<init>" $ MethodSignature [bifunctionClass, IntType, arrayOf hoCls] ReturnsVoid
bifunctionApply :: NameType Method
bifunctionApply = NameType "apply" $ MethodSignature [] (Returns bifunctionClass)
clone :: NameType Method
clone = NameType "clone" $ MethodSignature [] (Returns Java.Lang.objectClass)
toString :: NameType Method
toString = NameType "toString" $ MethodSignature [] (Returns Java.Lang.stringClass)
makeInt :: Converter (NameType Method)
makeInt = intClass <&> \cls -> NameType "_make_Int" $ MethodSignature [IntType] (Returns cls)
makeInteger :: Converter (NameType Method)
makeInteger = integerClass <&> \cls -> NameType "_make_Integer" $ MethodSignature [Java.Lang.stringClass] (Returns cls)
makeChar :: Converter (NameType Method)
makeChar = charClass <&> \cls -> NameType "_make_Char" $ MethodSignature [CharByte] (Returns cls)
boxedDataBranch :: NameType Field
boxedDataBranch = NameType "branch" IntType
boxedDataData :: Converter (NameType Field)
boxedDataData = heapObjectClass <&> \cls -> NameType "data" $ arrayOf cls

-- |Create a new public static field in this class with the given name and type.
-- The given action will be run at the start of `main`, and should be used to initialise the field
makePublicStaticField :: Text -> FieldType -> (ClassFile.NameType Field -> Converter ()) -> Converter (ClassFile.NameType Field)
makePublicStaticField name fieldType init = do
    field <- newField [ ACC_PUBLIC, ACC_STATIC ] (toLazyByteString name) fieldType
    modify $ \s -> s
        { initialisers = initialisers s <> [init field]
        , topLevelSymbols = M.insert (VariableName name) fieldType (topLevelSymbols s) }
    return field

-- Create a function that wraps a Function object around an implementation (java) function
compileMakerFunction :: Text -> Int -> Int -> Text -> Converter (ClassFile.NameType Method)
compileMakerFunction name arity numFreeVars implName = do
    let accs = [ACC_PUBLIC, ACC_STATIC]
    wrapperArgs <- replicateM numFreeVars heapObjectClass
    funClass <- functionClass
    inScope $ newMethod accs (toLazyByteString name) wrapperArgs (Returns funClass) $ do
        setLocalVarCounter (fromIntegral numFreeVars) -- Local variables [0..numFreeVars) are used for arguments
        -- Record that we're using a dynamic function invocation, remember the implementation function's name
        methodIndex <- addDynamicMethod implName
        -- This is the function we're going to return at the end
        new =<< function
        dup
        -- Invoke the bootstrap method for this dynamic call site to create our BiFunction instance
        invokeDynamic methodIndex bifunctionApply
        -- Push the arity of the function
        pushInt arity
        -- Create an array of HeapObjects holding the free variables we were given as arguments
        pushInt numFreeVars
        allocNewArray =<< heapObject
        forM_ [0..numFreeVars - 1] $ \fv -> do
            -- For each free variable argument, push it into the same index in the array
            dup
            pushInt fv
            loadLocal (fromIntegral fv)
            aastore
        -- Call the Function constructor with the bifunction, the arity, and the free variable array
        liftJoin2 invokeSpecial function functionInit
        i0 ARETURN

-- |Consume initialisers from the state list until there are none left. We iterate them one at a time in case an
-- initialiser adds more initialisers to the list: we want to process these as well.
performInitialisers :: Converter ()
performInitialisers = gets initialisers >>= \case
    [] -> return ()
    init:inits -> do
        modify $ \s -> s { initialisers = inits }
        init
        performInitialisers

pushArg :: Arg -> Converter ()
pushArg (ArgLit l) = pushLit l
pushArg (ArgVar v) = pushSymbol v

pushLit :: Literal -> Converter ()
pushLit (LiteralInt i)    = do
    loadString $ unpack $ showt i
    liftJoin2 invokeStatic integer makeInteger
pushLit (LiteralChar c)   = do
    -- Push the integer equivalent char then JVM cast to char
    pushInt (fromEnum c)
    i2c
    liftJoin2 invokeStatic char makeChar

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
    storeSpecificUnnamedLocal i
    addLocalVariable v i

storeUnnamedLocal :: Converter LocalVar
storeUnnamedLocal = do
    localVar <- getFreshLocalVar
    storeSpecificUnnamedLocal localVar
    return localVar

storeUnnamedLocalInt :: Converter LocalVar
storeUnnamedLocalInt = do
    localVar <- getFreshLocalVar
    storeSpecificUnnamedLocalInt localVar
    return localVar

storeSpecificUnnamedLocal :: MonadGenerator m => LocalVar -> m ()
storeSpecificUnnamedLocal = storeSpecificUnnamedLocal' astore_ astore astorew

storeSpecificUnnamedLocalInt :: MonadGenerator m => LocalVar -> m ()
storeSpecificUnnamedLocalInt = storeSpecificUnnamedLocal' istore_ istore istorew

storeSpecificUnnamedLocal' :: MonadGenerator m => (IMM -> m ()) -> (Word8 -> m ()) -> (Word16 -> m ()) -> LocalVar -> m ()
storeSpecificUnnamedLocal' storeImm store storew i = case i of
    0 -> storeImm I0
    1 -> storeImm I1
    2 -> storeImm I2
    3 -> storeImm I3
    _ -> if i < 256 then store $ fromIntegral i else storew $ fromIntegral i

loadLocal :: MonadGenerator m => LocalVar -> m ()
loadLocal = loadLocal' aload_ aload aloadw

loadLocalInt :: MonadGenerator m => LocalVar -> m ()
loadLocalInt = loadLocal' iload_ iload iloadw

loadLocal' :: MonadGenerator m => (IMM -> m ()) -> (Word8 -> m ()) -> (Word16 -> m ()) -> LocalVar -> m ()
loadLocal' loadImm load loadw i = case i of
    0 -> loadImm I0
    1 -> loadImm I1
    2 -> loadImm I2
    3 -> loadImm I3
    _ -> if i < 256 then load $ fromIntegral i else loadw $ fromIntegral i

-- |Given a java-land Boolean on the top of the stack, convert it to a haskell-land True/False BoxedData value
makeBoxedBool :: Converter ()
makeBoxedBool = do
    makeTrue <- getMakeMethod "True" [] (Returns <$> boolClass)
    makeFalse <- getMakeMethod "False" [] (Returns <$> boolClass)
    let invokeStaticBytes = 3
        jumpBytes = 3
    i0 (IF C_EQ $ jumpBytes + invokeStaticBytes + jumpBytes)
    -- If the value is 1, make true and jump past the other case
    liftJoin2 invokeStatic bool (pure makeTrue)
    goto (jumpBytes + invokeStaticBytes)
    -- If the value is 0, make false
    liftJoin2 invokeStatic bool (pure makeFalse)

-- |Given a Java-land string on the top of the stack, convert it to a Haskell-land list of haskell-land chars.
makeBoxedString :: Converter ()
makeBoxedString = do
    makeNil <- getMakeMethod "[]" [] (Returns <$> listClass)
    makeCons <- getMakeMethod ":" [heapObjectClass, heapObjectClass] (Returns <$> listClass)
    let charAt = NameType "charAt" $ MethodSignature [IntType] (Returns CharByte)

    -- Store the given string as a local variable
    stringVar <- storeUnnamedLocal
    -- Create an empty list to store the haskell-land string
    liftJoin2 invokeStatic list (pure makeNil)
    listVar <- storeUnnamedLocal
    -- Loop over the string, building up a Haskell list of it
    -- Create the index, starting at the length of the string and looping backwards over it
    loadLocal stringVar
    invokeVirtual Java.Lang.string $ NameType "length" $ MethodSignature [] (Returns IntType)
    index <- storeUnnamedLocalInt
    -- Loop condition: if the index is == 0, jump out of the loop
    charCls <- char
    makeCharFun <- makeChar
    listCls <- list
    let body = do
            loadLocal stringVar
            loadLocalInt index
            iconst_1
            isub
            dup
            storeSpecificUnnamedLocalInt index
            invokeVirtual Java.Lang.string charAt
            invokeStatic charCls makeCharFun
            loadLocal listVar
            invokeStatic listCls makeCons
            storeSpecificUnnamedLocal listVar
            -- There should be a goto here but we skip it as we can't compute the offset yet
    bodyLength' <- Converter (lift $ getGenLength 0 [body]) >>= \case
        [l] -> return l
        x -> throwTextError $ "Illegal result from getGenlength: " <> showt x
    let bodyLength = fromIntegral bodyLength' + 3 -- Jump past the goto taking us back to the top of the loop
    loadLocalInt index
    i0 $ IF C_EQ (bodyLength + 3)
    Converter $ lift body
    goto $ -(bodyLength + 2) -- Jump to loading the index before the IFEQ
    -- After loop: load the resulting haskell-land string onto the stack
    loadLocal listVar

makeUnboxedString :: Converter ()
makeUnboxedString = (,) <$> gets (M.lookup "[]" . topLevelRenames) <*> gets (M.lookup "[]" . datatypes) >>= \case
    (Nothing, _) -> throwTextError "Can't find []"
    (_, Nothing) -> throwTextError "No datatype []"
    (Just nilName, Just d) -> case M.lookupIndex (jvmSanitise nilName) $ branches d of
        Nothing -> throwTextError $ "No branch " <> showt (jvmSanitise nilName)
        Just nilIndex -> do
            listCls <- list
            charCls <- char
            heapObjectCls <- heapObject
            enterMeth <- enter
            boxedDataDataField <- boxedDataData
            let stringBuilder = "java/lang/StringBuilder"
                sbAppend = NameType "append" $ MethodSignature [CharByte] (Returns $ ObjectType $ unpack $ fromLazyByteString stringBuilder)
            -- Store the top-of-stack haskell-land string
            listVar <- storeUnnamedLocal
            -- Create a new stringbuilder, store it
            new stringBuilder
            dup
            invokeSpecial stringBuilder Java.Lang.objectInit
            builderVar <- storeUnnamedLocal
            let body = do
                    loadLocal builderVar
                    loadLocal listVar
                    invokeVirtual heapObjectCls enterMeth
                    checkCast listCls
                    getField listCls boxedDataDataField
                    dup
                    -- Load the rest of the list, overwrite our reference
                    iconst_1
                    aaload
                    storeSpecificUnnamedLocal listVar
                    -- Load the head char, append to the builder, pop the returned reference
                    iconst_0
                    aaload
                    checkCast charCls
                    getField charCls $ NameType "value" CharByte
                    invokeVirtual stringBuilder sbAppend
                    pop
                    -- Should be a jump back to the top of the loop here, we insert it after
            bodyLength' <- Converter (lift $ getGenLength 0 [body]) >>= \case
                [l] -> return l
                x -> throwTextError $ "Illegal result from getGenlength: " <> showt x
            let bodyLength = fromIntegral bodyLength' + 3 -- Adding goto offset
            -- Head of the loop
            loadLocal listVar
            invokeVirtual heapObjectCls enterMeth
            checkCast listCls
            getField listCls boxedDataBranch
            pushInt nilIndex
            i0 $ IF_ICMP C_EQ (bodyLength + 3) -- We're at the end of the list, jump past the body
            Converter $ lift body
            goto $ -(bodyLength + 11) -- Jump to the head
            -- Out of the loop, load the stringbuilder and get our final string
            loadLocal builderVar
            invokeVirtual stringBuilder toString


getMakeMethod :: VariableName -> [Converter FieldType] -> Converter ReturnSignature -> Converter (NameType Method)
getMakeMethod v args ret = gets (M.lookup v . topLevelRenames) >>= \case
    Nothing -> throwTextError $ "Missing renaming for " <> showt v <> " in CodeGen"
    Just renamed -> do
        sig <- MethodSignature <$> sequence args <*> ret
        return $ NameType (toLazyByteString $ convertName $ "_make" <> jvmSanitise renamed) sig


printTopOfStack :: MonadGenerator m => m ()
printTopOfStack = do
    dup
    getStaticField Java.Lang.system Java.IO.out
    JVM.Builder.swap
    invokeVirtual Java.Lang.object toString
    invokeVirtual Java.IO.printStream Java.IO.println
printText :: MonadGenerator m => Text -> m ()
printText t = do
    getStaticField Java.Lang.system Java.IO.out
    loadString $ unpack t
    invokeVirtual Java.IO.printStream Java.IO.println
