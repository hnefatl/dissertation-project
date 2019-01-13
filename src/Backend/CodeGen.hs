{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Backend.CodeGen where

import           BasicPrelude                hiding (encodeUtf8, head)
import           Control.Monad.Except        (Except, ExceptT, runExcept, runExceptT, throwError, withExceptT)
import           Control.Monad.State.Strict  (MonadState, StateT, evalStateT, execStateT, gets, modify, get)
import qualified Data.ByteString.Lazy        as B
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           Data.Text                   (unpack, pack)
import           Data.Text.Lazy              (fromStrict, toStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Data.Word                   (Word16)
import           System.FilePath             ((<.>), (</>))
import           TextShow                    (TextShow, showb, showt)
import           Data.Default                (Default, def)
import           Data.Maybe                  (fromJust)
import qualified Data.Sequence               as Seq
import qualified Data.Binary                 as Binary
import           Data.Foldable               (toList)

import           Java.ClassPath
import           JVM.Assembler
import           JVM.Builder
import           JVM.Builder.Monad           (runGeneratorT, execGeneratorT, encodedCodeLength, classPath)
import qualified JVM.ClassFile               as ClassFile
import           JVM.ClassFile               hiding (Class, Method)
import           JVM.Converter

import           Backend.ILA                 (Alt(..), Binding(..), Literal(..), AltConstructor(..), isDefaultAlt, isDataAlt, isLiteralAlt)
import           Backend.ILB                 hiding (Converter, ConverterState)
import           NameGenerator
import           Names                       (VariableName, TypeVariableName, convertName)
import           Preprocessor.ContainedNames
import           ExtraDefs                   (zipOverM_)

type Class = ClassFile.Class ClassFile.Direct
type OutputClass = ClassFile.Class ClassFile.File
type Method = ClassFile.Method ClassFile.Direct

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


data ConverterState = ConverterState
    { -- Actions to push a variable onto the stack
      variablePushers :: M.Map VariableName (Converter ())
    , -- Which JVM local variable we're up to
      localVarCounter :: Word16
    , -- Map from datatype constructor name to branch number, eg. {False:0, True:1} or {Nothing:0, Just:1}
      datatypes :: M.Map TypeVariableName Datatype
    , -- Which methods we're using an invokeDynamic instruction on, so we need to add bootstrap methods for
      dynamicMethods :: Seq.Seq Text  }
instance Default ConverterState where
    def = ConverterState
        { variablePushers = M.empty
        , localVarCounter = 0
        -- TODO(kc506): Cheatily hardcoded for now, fill in given more datatype information
        , datatypes = M.fromList
            [ ("Bool", Datatype "Bool" ["False", "True"])
            , ("Maybe", Datatype "Maybe" ["Nothing", "Just"]) ]
        , dynamicMethods = Seq.empty }

-- A `Converter Method` action represents a computation that compiles a method: the ConverterState should be reset
-- inbetween method compilations
newtype Converter a = Converter (StateT ConverterState (GeneratorT (ExceptT Text (NameGeneratorT IO))) a)
    deriving (Functor, Applicative, Monad, MonadState ConverterState, MonadGenerator, MonadNameGenerator, MonadIO)

-- |`convert` takes a class name, a path to the primitive java classes, and a list of ILB bindings and produces a class
-- file
convert :: Text -> FilePath -> [Binding Rhs] -> ExceptT Text (NameGeneratorT IO) OutputClass
convert classname primitiveClassDir bs = withExceptT (\e -> unlines [e, showt bs]) action
    where action = addBootstrapMethods inner (encodeUtf8 $ fromStrict classname)
          processBinding (NonRec v (RhsClosure vs body)) = compileStaticMethod v vs body
          processBinding (Rec m) = mapM_ (\(v,r) -> processBinding $ NonRec v r) (M.toList m)
          inner = do
            loadPrimitiveClasses primitiveClassDir
            mapM_ processBinding bs

evalConverter :: Converter a -> ConverterState -> GeneratorT (ExceptT Text (NameGeneratorT IO)) a
evalConverter (Converter x) = evalStateT x

throwTextError :: Text -> Converter a
throwTextError = liftErrorText . throwError
liftErrorText :: Except Text a -> Converter a
liftErrorText x = case runExcept x of
    Left e  -> Converter $ lift $ lift $ throwError e
    Right y -> return y
liftExc :: ExceptT Text (NameGeneratorT IO) a -> Converter a
liftExc = Converter . lift . lift

setLocalVarCounter :: Word16 -> Converter ()
setLocalVarCounter x = modify (\s -> s { localVarCounter = x })
getFreshLocalVar :: Converter Word16
getFreshLocalVar = do
    x <- gets localVarCounter
    when (x == maxBound) $ throwTextError "Maximum number of local variables met"
    modify (\s -> s { localVarCounter = x + 1 })
    return x

addVariablePusher :: VariableName -> Converter () -> Converter ()
addVariablePusher v = addVariablePushers . M.singleton v
addVariablePushers :: M.Map VariableName (Converter ()) -> Converter ()
addVariablePushers m = modify (\s -> s { variablePushers = M.union m (variablePushers s) })
pushVariable :: VariableName -> Converter ()
pushVariable v = gets (M.lookup v . variablePushers) >>= \case
    Just pusher -> pusher
    Nothing -> throwTextError $ "No pusher for variable " <> showt v

addDynamicMethod :: Text -> Converter Word16
addDynamicMethod m = do
    methods <- gets dynamicMethods
    modify (\s -> s { dynamicMethods = dynamicMethods s Seq.|> m })
    return $ fromIntegral $ Seq.length methods

loadPrimitiveClasses :: FilePath -> Converter ()
loadPrimitiveClasses dirPath = withClassPath $ mapM_ (loadClass . (dirPath </>)) classes
    where classes = ["HeapObject", "Function"]

-- |We use an invokeDynamic instruction to pass functions around in the bytecode. In order to use it, we need to add a
-- bootstrap method for each function we create.
-- This should be called at the end of the generation of a class file
addBootstrapMethods :: Converter () -> B.ByteString -> ExceptT Text (NameGeneratorT IO) OutputClass
addBootstrapMethods x classname = runExceptT (generateT [] classname $ addBootstrapPoolItems x classname) >>= \case
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
addBootstrapPoolItems :: Converter () -> B.ByteString -> GeneratorT (ExceptT Text (NameGeneratorT IO)) (Word16, Word16, Word16, [Word16], Word16)
addBootstrapPoolItems (Converter x) classname = do
    converterState <- execStateT x def
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
                , methodName = encodeUtf8 $ fromStrict name
                , methodSignature = sig
                , methodAttributesCount = 0
                , methodAttributes = AR M.empty }
        addToPool (CMethodHandle InvokeStatic classname method)
    bmIndex <- addToPool (CUTF8 "BootstrapMethods")
    return (bmIndex, mfIndex, arg1, arg2s, arg3)

-- The support classes written in Java and used by the Haskell translation
heapObject, function, thunk, bifunction :: IsString s => s
heapObject = "HeapObject"
function = "Function"
thunk = "Thunk"
bifunction = "java/util/function/BiFunction"
heapObjectClass, functionClass, thunkClass, bifunctionClass :: FieldType
heapObjectClass = ObjectType heapObject
functionClass = ObjectType function
thunkClass = ObjectType thunk
bifunctionClass = ObjectType bifunction
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

-- TODO: Given that each `Converter Method` instance should be for compiling a full module, maybe do the unwrapping
-- inside here and return just an `ExceptT Text (NameGeneratorT IO) a`?
-- |Compile a function binding into bytecode. It's a bit complicated:
-- To compile a function `f` with free variables `fv` and arguments `as`, we create two functions.
-- - `_fImpl`, which takes two arrays of heap objects (first contains arguments, second contains free variables) and
--   returns a heap object containing the result of the application.
-- - `_makef`, which takes `|fv|` arguments matching the free variables, and returns a `Function` object.
-- To pass the `Bifunction` argument to the `Function` constructor, we need to perform an invokeDynamic instruction to
-- get a lambda from a static function.
compileStaticMethod :: VariableName -> [VariableName] -> Exp -> Converter ()
compileStaticMethod name args body = do
    setLocalVarCounter 2 -- The first two local variables (0 and 1) are reserved for the function arguments
    freeVariables <- liftErrorText $ getFreeVariables body
    -- Map each variable name to a pusher that pushes it onto the stack
    zipOverM_ (S.toAscList freeVariables) [0..] $ \v n -> addVariablePusher v $ do
        iconst_1
        pushInt n
        aaload
    zipOverM_ args [0..] $ \v n -> addVariablePusher v $ do
        iconst_0
        pushInt n
        aaload
    let implName = "_" <> fromStrict (convertName name) <> "Impl"
        implArgs = [arrayOf heapObjectClass, arrayOf heapObjectClass]
        implAccs = [ACC_PRIVATE, ACC_STATIC]
    -- The actual implementation of the function
    _ <- newMethod implAccs (encodeUtf8 implName) implArgs (Returns heapObjectClass) $ do
        compileExp body
        i0 ARETURN
    -- Reset some state before making the wrapper function
    modify (\s -> s { localVarCounter = undefined, variablePushers = M.empty })
    let wrapperName = encodeUtf8 $ "_make" <> fromStrict (convertName name)
        numFreeVars = S.size freeVariables
        arity = length args
        wrapperArgs = replicate numFreeVars heapObjectClass
        wrapperAccs = [ACC_PUBLIC, ACC_STATIC]
    -- The wrapper function to construct an application instance of the function
    _ <- newMethod wrapperAccs wrapperName wrapperArgs (Returns functionClass) $ do
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
    return ()

compileExp :: Exp -> Converter ()
compileExp (ExpLit l) = compileLit l
compileExp (ExpApp fun args) = do
    let pushFun = pushVariable fun
    -- Add each argument to the function object
    forM_ args $ \arg -> do
        pushFun
        compileArg arg
        invokeVirtual heapObject addArgument
    -- Construct a thunk around the fun
    new thunk
    dup
    pushFun
    invokeSpecial thunk thunkInit
compileExp (ExpConApp _ _) = throwTextError "Need support for data applications"
compileExp (ExpCase head vs alts) = do
    compileExp head
    -- Bind each extra variable to the head
    forM_ vs $ \var -> do
        localVar <- getFreshLocalVar -- Fresh local variable in the JVM
        dup -- Copy the head expression
        storeLocal localVar -- Store it locally
        addVariablePusher var $ loadLocal localVar -- Remember where we stored the variable
    compileCase alts
compileExp (ExpLet var (RhsClosure args body') body) = do
    localVar <- getFreshLocalVar
    -- TODO(kc506): Replace `Converter $ lift` with something more generic
    -- How to "get a function"? Use the _makef function, remember everything's a heapobject
    _ <- Converter $ lift $ evalConverter (compileStaticMethod var args body') def
    -- TODO(kc560): Need to construct a Thunk here?
    storeLocal localVar
    compileExp body

compileCase :: [Alt Exp] -> Converter ()
compileCase as = do
    let (defaultAlts, otherAlts) = partition isDefaultAlt as
        numAlts = genericLength otherAlts
    -- altKeys are the branch numbers of each alt's constructor
    (altKeys, alts) <- unzip <$> sortAlts otherAlts
    defaultAlt <- case defaultAlts of
        [x] -> return x
        x -> throwTextError $ "Expected single default alt in case statement, got: " <> showt x
    let defaultAltGenerator = (\(Alt _ _ e) -> compileExp e) defaultAlt
        altGenerators = map (\(Alt _ _ e) -> compileExp e) alts
    classPathEntries <- classPath <$> getGState
    let getAltLength :: (Converter ()) -> Converter Word32
        getAltLength e = do
            converterState <- get
            -- Run a compilation in a separate generator monad instance
            x <- liftExc $ runExceptT $ execGeneratorT classPathEntries $ evalConverter e converterState
            generatorState <- case x of
                Left err -> throwTextError (pack $ show err)
                Right s -> return s
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
                getDataCon _ = error "Compiler error: can only have datacons here"
                okay = all ((`elem` branches) . getDataCon) alts
            unless okay $ throwTextError "Alt mismatch: constructors from different types"
            let alts' = map (\a -> (fromIntegral $ fromJust $ (getDataCon a) `elemIndex` branches, a)) alts
            return $ sortOn fst alts'
sortAlts alts@(Alt (LitCon _) _ _:_) = do
    unless (all isLiteralAlt alts) $ throwTextError "Alt mismatch: expected all literal alts"
    throwTextError "Need to refactor literals to only be int/char before doing this"
sortAlts (Alt Default _ _:_) = throwTextError "Can't have Default constructor"


compileArg :: Arg -> Converter ()
compileArg (ArgLit l) = compileLit l
compileArg (ArgVar v) = pushVariable v

compileLit :: Literal -> Converter ()
compileLit (LiteralInt i) = pushInt (fromIntegral i)
compileLit (LiteralChar _) = throwTextError "Need support for characters"
compileLit (LiteralString _) = throwTextError "Need support for strings"
compileLit (LiteralFrac _) = throwTextError "Need support for rationals"

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

storeLocal :: Word16 -> Converter ()
storeLocal i
    | i < 256 = astore_ $ fromIntegral i
    | otherwise = wide ASTORE $ CInteger (fromIntegral i)

loadLocal :: Word16 -> Converter ()
loadLocal 0 = aload_ I0
loadLocal 1 = aload_ I1
loadLocal 2 = aload_ I2
loadLocal 3 = aload_ I3
loadLocal i = aload $ CInteger $ fromIntegral i