{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Backend.CodeGen where

import           BasicPrelude                hiding (encodeUtf8, head, init)
import           Control.Monad.Except        (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import           Control.Monad.State.Strict  (MonadState, StateT, execStateT, get, gets, modify)
import qualified Data.ByteString.Lazy        as B
import           Data.Foldable               (null, toList)
import           Data.Functor                (void)
import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromJust)
import qualified Data.Sequence               as Seq
import qualified Data.Set                    as S
import           Data.Text                   (pack, unpack)
import           Data.Text.Lazy              (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Data.Word                   (Word16)
import           System.FilePath             ((</>))
import           TextShow                    (TextShow, showb, showt)

import           Java.ClassPath
import qualified Java.IO
import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                 hiding (locals)
import           JVM.Builder.Monad           (classPath, encodedCodeLength, execGeneratorT)
import           JVM.ClassFile               hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile               as ClassFile
import           JVM.Converter

import           Backend.ILA                 (Alt(..), AltConstructor(..), Binding(..), Literal(..),
                                              getBindingVariables, isDataAlt, isDefaultAlt, isLiteralAlt)
import           Backend.ILB                 hiding (Converter, ConverterState)
import           ExtraDefs                   (zipOverM_)
import           Logger                      (LoggerT, MonadLogger, writeLog)
import           NameGenerator
import           Names                       (TypeVariableName, VariableName(..), convertName)
import qualified Preprocessor.ContainedNames as ContainedNames
import qualified Typechecker.Types           as Types
import           Typechecker.Hardcoded       (builtinFunctions, builtinDictionaries)

type Class = ClassFile.Class ClassFile.Direct
type OutputClass = ClassFile.Class ClassFile.File
type Method = ClassFile.Method ClassFile.Direct
type Field = ClassFile.Field ClassFile.Direct

data NamedClass = NamedClass Text OutputClass
    deriving (Eq, Show)
instance TextShow NamedClass where
    showb = fromString . show

--writeClass :: FilePath -> NamedClass -> IO ()
--writeClass directory (NamedClass name c) = B.writeFile (directory </> unpack name <.> "class") (encodeClass c)

-- TODO(kc506): Move somewhere more appropriate for when we generate this information
-- |Representation of a datatype: the type name eg. Maybe, and the list of contstructor names eg. Nothing, Just
data Datatype = Datatype TypeVariableName [VariableName]
    deriving (Eq, Ord, Show)

type LocalVar = Word16

toLazyBytestring :: Text -> B.ByteString
toLazyBytestring = encodeUtf8 . fromStrict

data ConverterState = ConverterState
    { -- Which JVM local variable we're up to
      localVarCounter :: LocalVar
    , -- Map from datatype constructor name to branch number, eg. {False:0, True:1} or {Nothing:0, Just:1}
      datatypes       :: M.Map TypeVariableName Datatype
    , -- Top-level variable names. Eg `x = 1 ; y = let z = 2 in z` has top-level variable names x and y. This is used when working out which free variables we need to pass to functions, and which it can get for itself.
      topLevelSymbols :: S.Set VariableName
    , -- A reverse mapping from the renamed top-level variables to what they were originally called.
      topLevelRenames :: M.Map VariableName VariableName
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

-- |`convert` takes a class name, a path to the primitive java classes, a list of ILB bindings, and the renames used for
-- the top-level symbols and produces a class file
convert :: Text -> FilePath -> [Binding Rhs] -> M.Map VariableName VariableName -> ExceptT Text (LoggerT (NameGeneratorT IO)) OutputClass
convert cname primitiveClassDir bs topLevelRenamings = do
    writeLog "-----------"
    writeLog "- CodeGen -"
    writeLog "-----------"
    withExceptT (\e -> unlines [e, showt bs]) action
    where initialState = ConverterState
            { localVarCounter = 0
            , datatypes = M.fromList
                [ ("Bool", Datatype "Bool" ["False", "True"])
                , ("Maybe", Datatype "Maybe" ["Nothing", "Just"]) ]
            , topLevelSymbols = S.unions $ M.keysSet builtinFunctions:map getBindingVariables bs
            , topLevelRenames = topLevelRenamings
            , localSymbols = M.empty
            , initialisers = []
            , dynamicMethods = Seq.empty
            , dictionaries = S.singleton $ Types.IsInstance "Num" Types.typeInt  --M.keysSet builtinDictionaries
            , classname = toLazyBytestring cname }
          action = addBootstrapMethods inner initialState
          processBinding (NonRec v rhs) = void $ compileGlobalClosure v rhs
          processBinding (Rec m)        = mapM_ (\(v,r) -> processBinding $ NonRec v r) (M.toList m)
          inner = do
            compileDictionaries
            loadPrimitiveClasses primitiveClassDir
            addMainMethod
            mapM_ processBinding bs

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
    case (S.member v globals, M.member v locals) of
        (False, False) -> throwTextError $ "Variable " <> showt v <> " not found in locals or globals"
        (True, True)   -> throwTextError $ "Variable " <> showt v <> " found in both locals and globals"
        (True, False)  -> pushGlobalSymbol v
        (False, True)  -> pushLocalSymbol v
pushGlobalSymbol :: VariableName -> Converter ()
pushGlobalSymbol v = do
    writeLog $ "Pushing " <> showt v <> " from globals"
    cname <- gets classname
    renames <- gets topLevelRenames
    getStaticField cname $ publicStaticField $ convertName v
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

freeVariables :: ContainedNames.HasFreeVariables a => VariableName -> [VariableName] -> a -> Converter (S.Set VariableName)
freeVariables name args x = do
    fvs <- liftErrorText $ ContainedNames.getFreeVariables x
    tls <- gets topLevelSymbols
    -- TODO(kc506): remove these when we remove hardcoded stuff
    let bfs = M.keysSet builtinFunctions
    return $ S.difference fvs (S.unions [S.singleton name, S.fromList args, tls, bfs])

addDynamicMethod :: Text -> Converter Word16
addDynamicMethod m = do
    methods <- gets dynamicMethods
    modify (\s -> s { dynamicMethods = dynamicMethods s Seq.|> m })
    return $ fromIntegral $ Seq.length methods

loadPrimitiveClasses :: FilePath -> Converter ()
loadPrimitiveClasses dirPath = withClassPath $ mapM_ (loadClass . (dirPath </>)) classes
    where classes = ["HeapObject", "Function"]

addMainMethod :: Converter ()
addMainMethod = do
    let access = [ ACC_PUBLIC, ACC_STATIC ]
    void $ newMethod access "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        -- Perform any initialisation actions
        sequence_ =<< gets initialisers
        getStaticField Java.Lang.system Java.IO.out
        pushSymbol "_main"
        invokeVirtual heapObject enter
        invokeVirtual heapObject toString
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN

-- |Create a new class for each datatype
compileDatatypes :: [Datatype] -> ExceptT Text (LoggerT (NameGeneratorT IO)) [OutputClass]
compileDatatypes ds = throwError "Need to implement datatype compilation"

compileDictionaries :: Converter ()
compileDictionaries = do
    dicts <- gets dictionaries
    cname <- gets classname
    -- For each dictionary, create a new static field
    -- TODO(kc506): Create the dictionary functions in this class
    forM_ dicts $ \(Types.IsInstance c t) -> do
        -- TODO(kc506): Verify the name's acceptable (like `NumInt`, not `Num[a]`)
        let name = showt c <> showt t
            datatypeClassName = "_" <> name
            datatype = ObjectType $ unpack datatypeClassName
        makePublicStaticField ("d" <> name) datatype $ \field -> do
            let method = ClassFile.NameType (toLazyBytestring $ "_make" <> name) $ MethodSignature [] (Returns datatype)
            invokeStatic (toLazyBytestring datatypeClassName) method
            putStaticField cname field

-- |We use an invokeDynamic instruction to pass functions around in the bytecode. In order to use it, we need to add a
-- bootstrap method for each function we create.
-- This should be called at the end of the generation of a class file
addBootstrapMethods :: Converter () -> ConverterState -> ExceptT Text (LoggerT (NameGeneratorT IO)) OutputClass
addBootstrapMethods x s = runExceptT (generateT [] (classname s) $ addBootstrapPoolItems x s) >>= \case
    Left err -> throwError $ pack $ show err
    Right ((bootstrapMethodStringIndex, metafactoryIndex, arg1, arg2s, arg3), classDirect) -> do
        let classFile = classDirect2File classDirect -- Convert the "direct" in-memory class into a class file structure
            bootstrapMethods = [ BootstrapMethod { bootstrapMethodRef = metafactoryIndex, bootstrapArguments = [ arg1, arg2, arg3 ] } | arg2 <- arg2s ]
            bootstrapMethodsAttribute = toAttribute $ BootstrapMethodsAttribute
                { attributeNameIndex = bootstrapMethodStringIndex
                , attributeMethods = bootstrapMethods }
        return $ classFile
            { classAttributesCount = classAttributesCount classFile + 1
            , classAttributes = AP $ bootstrapMethodsAttribute:attributesList (classAttributes classFile) }
addBootstrapPoolItems :: Converter () -> ConverterState -> GeneratorT (ExceptT Text (LoggerT (NameGeneratorT IO))) (Word16, Word16, Word16, [Word16], Word16)
addBootstrapPoolItems (Converter x) s = do
    converterState <- execStateT x s
    -- Add the LambdaMetafactory metafactory method to the pool (used to get lambdas from static functions)
    let metafactoryArgs = map ObjectType
            ["java/lang/invoke/MethodHandles$Lookup", "java/lang/String", "java/lang/invoke/MethodType", "java/lang/invoke/MethodType", "java/lang/invoke/MethodHandle", "java/lang/invoke/MethodType" ]
        metafactorySig = MethodSignature metafactoryArgs (Returns $ ObjectType "java/lang/invoke/CallSite")
        metafactoryMethod = ClassFile.Method
            { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
            , methodName = "metafactory"
            , methodSignature = metafactorySig
            , methodAttributesCount = 0
            , methodAttributes = AR M.empty
            }
    mfIndex <- addToPool (CMethodHandle InvokeStatic "java/lang/invoke/LambdaMetafactory" metafactoryMethod)
    -- Add some constants to the pool that we'll need later, when we're creating the bootstrap method attribute
    arg1 <- addToPool (CMethodType "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
    arg3 <- addToPool (CMethodType "([LHeapObject;[LHeapObject;)LHeapObject;")
    let methods = dynamicMethods converterState
    arg2s <- forM (toList methods) $ \name -> do
        let sig = MethodSignature [arrayOf heapObjectClass, arrayOf heapObjectClass] (Returns heapObjectClass)
            method = ClassFile.Method
                { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
                , methodName = toLazyBytestring name
                , methodSignature = sig
                , methodAttributesCount = 0
                , methodAttributes = AR M.empty }
        addToPool (CMethodHandle InvokeStatic (classname s) method)
    bmIndex <- addToPool (CUTF8 "BootstrapMethods")
    return (bmIndex, mfIndex, arg1, arg2s, arg3)

-- The support classes written in Java and used by the Haskell translation
heapObject, function, thunk, bifunction, unboxedData, boxedData :: IsString s => s
heapObject = "HeapObject"
function = "Function"
thunk = "Thunk"
bifunction = "java/util/function/BiFunction"
unboxedData = "Data"
boxedData = "BoxedData"
heapObjectClass, functionClass, thunkClass, bifunctionClass, unboxedDataClass, boxedDataClass  :: FieldType
heapObjectClass = ObjectType heapObject
functionClass = ObjectType function
thunkClass = ObjectType thunk
bifunctionClass = ObjectType bifunction
unboxedDataClass = ObjectType unboxedData
boxedDataClass = ObjectType boxedData
addArgument :: NameType Method
addArgument = ClassFile.NameType "addArgument" $ MethodSignature [heapObjectClass] ReturnsVoid
enter :: NameType Method
enter = NameType "enter" $ MethodSignature [] (Returns heapObjectClass)
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

-- |Create a new public static field in this class with the given name and type.
-- The given action will be run at the start of `main`, and should be used to initialise the field
makePublicStaticField :: Text -> FieldType -> (ClassFile.NameType Field -> Converter ()) -> Converter (ClassFile.NameType Field)
makePublicStaticField name fieldType init = do
    field <- newField [ ACC_PUBLIC, ACC_STATIC ] (toLazyBytestring name) fieldType
    modify $ \s -> s
        { initialisers = init field:initialisers s
        , topLevelSymbols = S.insert (VariableName name) (topLevelSymbols s) }
    return field

publicStaticField :: Text -> ClassFile.NameType Field
publicStaticField name = NameType
    { ntName = toLazyBytestring name
    , ntSignature = thunkClass }

-- |Compiling a global closure results in a static class field with the given name set to either a `Thunk` or a `Function`
compileGlobalClosure :: VariableName -> Rhs -> Converter (ClassFile.NameType Field)
compileGlobalClosure v (RhsClosure [] body) =
    -- As this symbol is global, we store it in a static class field
    makePublicStaticField (convertName v) (ObjectType heapObject) $ \field -> do
        -- We're compiling a non-function, so we can just make a thunk
        compileThunk body
        cname <- gets classname
        putStaticField cname field
compileGlobalClosure v (RhsClosure args body) = do
    -- We're compiling a global function, so it shouldn't have any non top-level free variables
    fvs <- freeVariables v args body
    unless (null fvs) $ throwTextError $ unlines ["Top-level function has free variables: " <> showt v, showt fvs, showt args, showt body]
    -- We compile the function to get an implementation and _makeX function (f is a handle on the _makeX function)
    f <- compileFunction v args body
    -- Get a function instance using f, then store it as a static field
    makePublicStaticField (convertName v) (ObjectType heapObject) $ \field -> do
        cname <- gets classname
        invokeStatic cname f
        putStaticField cname field
-- |Compiling a local closure results in local variable bindings to either a `Thunk` or a `Function`.
compileLocalClosure :: VariableName -> Rhs -> Converter LocalVar
compileLocalClosure v (RhsClosure [] body) = do
    -- Same as the global closure, but storing in a local variable rather than a static field
    compileThunk body
    storeLocal v
compileLocalClosure v (RhsClosure args body) = do
    -- We compile the function to get an implementation and _makeX function (f is a handle on the _makeX function)
    f <- compileFunction v args body
    fvs <- freeVariables v args body
    -- Push each free variable used in the function onto the stack
    forM_ (S.toAscList fvs) pushLocalSymbol
    -- Consume the free variables to produce a Function object, then store it as a local variable
    cname <- gets classname
    invokeStatic cname f
    storeLocal v

-- |Compile an expression wrapped in a thunk, leaving the thunk on the top of the stack
compileThunk :: Exp -> Converter ()
compileThunk body = do
    new thunk
    dup
    compileExp body
    invokeSpecial thunk thunkInit

-- |Compile a function binding into bytecode. It's a bit complicated:
-- To compile a function `f` with free variables `fv` and arguments `as`, we create two functions.
-- - `_fImpl`, which takes two arrays of heap objects (first contains arguments, second contains free variables) and
--   returns a heap object containing the result of the application.
-- - `_makef`, which takes `|fv|` arguments matching the free variables, and returns a `Function` object.
-- To pass the `Bifunction` argument to the `Function` constructor, we need to perform an invokeDynamic instruction to
-- get a lambda from a static function.
compileFunction :: VariableName -> [VariableName] -> Exp -> Converter (ClassFile.NameType Method)
compileFunction name args body = do
    freeVars <- freeVariables name args body
    writeLog $ "Function " <> showt name <> " has free variables " <> showt freeVars
    let implName = "_" <> fromStrict (convertName name) <> "Impl"
        implArgs = [arrayOf heapObjectClass, arrayOf heapObjectClass]
        implAccs = [ACC_PRIVATE, ACC_STATIC]
    inScope $ do
        setLocalVarCounter 2 -- The first two local variables (0 and 1) are reserved for the function arguments
        -- Map each variable name to a pusher that pushes it onto the stack
        zipOverM_ args [0..] $ \v n -> addComplexLocalVariable v $ do
            loadLocal 0 -- Load array parameter
            pushInt n
            aaload -- Load value from array
        zipOverM_ (S.toAscList freeVars) [0..] $ \v n -> addComplexLocalVariable v $ do
            loadLocal 1
            pushInt n
            aaload
        -- The actual implementation of the function
        void $ newMethod implAccs (encodeUtf8 implName) implArgs (Returns heapObjectClass) $ do
            compileExp body
            i0 ARETURN
    let wrapperName = toLazyBytestring $ "_make" <> convertName name
        numFreeVars = S.size freeVars
        arity = length args
        wrapperArgs = replicate numFreeVars heapObjectClass
        wrapperAccs = [ACC_PUBLIC, ACC_STATIC]
    -- The wrapper function to construct an application instance of the function
    inScope $ newMethod wrapperAccs wrapperName wrapperArgs (Returns functionClass) $ do
        setLocalVarCounter (fromIntegral numFreeVars) -- Local variables [0..numFreeVars) are used for arguments
        -- Record that we're using a dynamic function invocation, remember the implementation function's name
        methodIndex <- addDynamicMethod (toStrict implName)
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

compileExp :: Exp -> Converter ()
compileExp (ExpVar v) = pushSymbol v
compileExp (ExpLit l) = pushLit l
compileExp (ExpApp fun args) = do
    pushSymbol fun -- Grab the function
    invokeVirtual heapObject clone -- Copy it so we don't mutate the reference version
    checkCast function -- Cast the returned Object to a Function
    -- Add each argument to the function object
    forM_ args $ \arg -> do
        dup
        pushArg arg
        invokeVirtual function addArgument
    invokeVirtual function enter
compileExp (ExpConApp _ _) = throwTextError "Need support for data applications"
compileExp (ExpCase head vs alts) = do
    compileExp head
    -- Bind each extra variable to the head
    forM_ vs $ \var -> do
        dup -- Copy the head expression
        storeLocal var -- Store it locally
    -- Compile the actual case expression
    compileCase alts
compileExp (ExpLet var rhs body) = do
    -- Compile the bound expression, store it in a local
    void $ compileLocalClosure var rhs
    -- Compile the inner expression
    compileExp body

compileCase :: [Alt Exp] -> Converter ()
compileCase as = do
    let (defaultAlts, otherAlts) = partition isDefaultAlt as
        numAlts = genericLength otherAlts
    -- altKeys are the branch numbers of each alt's constructor
    (altKeys, alts) <- unzip <$> sortAlts otherAlts
    defaultAlt <- case defaultAlts of
        [x] -> return x
        x   -> throwTextError $ "Expected single default alt in case statement, got: " <> showt x
    let defaultAltGenerator = (\(Alt _ _ e) -> compileExp e) defaultAlt
        altGenerators = map (\(Alt _ _ e) -> compileExp e) alts
    classPathEntries <- classPath <$> getGState
    let getAltLength :: Converter () -> Converter Word32
        getAltLength (Converter e) = do
            converterState <- get
            -- Run a compilation in a separate generator monad instance
            x <- liftExc $ runExceptT $ execGeneratorT classPathEntries $ execStateT e converterState
            generatorState <- case x of
                Left err -> throwTextError (pack $ show err)
                Right s  -> return s
            return $ encodedCodeLength generatorState
    -- The lengths of each branch: just the code for the expressions, nothing else
    defaultAltLength <- getAltLength defaultAltGenerator
    altLengths <- mapM getAltLength altGenerators
    currentByte <- encodedCodeLength <$> getGState
    let -- Compute the length of the lookupswitch instruction, so we know how far to offset the jumps by
        instructionPadding = fromIntegral $ 4 - (currentByte `mod` 4)
        -- 1 byte for instruction, then padding, then 4 bytes for the default case and 8 bytes for each other case
        instructionLength = fromIntegral $ 1 + instructionPadding + 4 + 8 * numAlts
        -- The offsets past the switch instruction of each branch
        defaultAltOffset = instructionLength -- Default branch starts immediately after the instruction
        altOffsets = scanl (+) defaultAltLength altLengths -- Other branches come after the default branch
    -- TODO(kc506): Switch between using lookupswitch/tableswitch depending on how saturated the possible branches are
    let switch = LOOKUPSWITCH instructionPadding defaultAltOffset (fromIntegral numAlts) (zip altKeys altOffsets)
    i0 switch
    defaultAltGenerator
    sequence_ altGenerators


-- |Sort Alts into order of compilation, tagging them with the key to use in the switch statement
sortAlts :: [Alt a] -> Converter [(Word32, Alt a)]
sortAlts [] = return []
sortAlts alts@(Alt (DataCon con) _ _:_) = do
    unless (all isDataAlt alts) $ throwTextError "Alt mismatch: expected all data alts"
    ds <- gets datatypes
    -- Find the possible constructors by finding a datatype whose possible branches contain the first alt's constructor
    case find (con `elem`) (map (\(Datatype _ bs) -> bs) $ M.elems ds) of
        Nothing -> throwTextError "Unknown data constructor"
        Just branches -> do
            let getDataCon (Alt (DataCon c) _ _) = c
                getDataCon _                     = error "Compiler error: can only have datacons here"
                okay = all ((`elem` branches) . getDataCon) alts
            unless okay $ throwTextError "Alt mismatch: constructors from different types"
            let alts' = map (\a -> (fromIntegral $ fromJust $ (getDataCon a) `elemIndex` branches, a)) alts
            return $ sortOn fst alts'
sortAlts alts@(Alt (LitCon _) _ _:_) = do
    unless (all isLiteralAlt alts) $ throwTextError "Alt mismatch: expected all literal alts"
    throwTextError "Need to refactor literals to only be int/char before doing this"
sortAlts (Alt Default _ _:_) = throwTextError "Can't have Default constructor"


pushArg :: Arg -> Converter ()
pushArg (ArgLit l) = pushLit l
pushArg (ArgVar v) = pushSymbol v

pushLit :: Literal -> Converter ()
pushLit (LiteralInt i)    = pushInt (fromIntegral i)
pushLit (LiteralChar _)   = throwTextError "Need support for characters"
pushLit (LiteralString _) = throwTextError "Need support for strings"
pushLit (LiteralFrac _)   = throwTextError "Need support for rationals"

pushInt :: Int -> Converter ()
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
    | otherwise = throwTextError "Need support for: 32 bit integers and arbitrary-sized integers"

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

loadLocal :: Word16 -> Converter ()
loadLocal 0 = aload_ I0
loadLocal 1 = aload_ I1
loadLocal 2 = aload_ I2
loadLocal 3 = aload_ I3
loadLocal i = if i < 256 then aload $ fromIntegral i else aloadw $ fromIntegral i