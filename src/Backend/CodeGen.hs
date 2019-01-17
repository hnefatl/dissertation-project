{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Backend.CodeGen where

import           BasicPrelude                hiding (encodeUtf8, head, init)
import           Control.Monad.Except        (ExceptT, runExceptT, throwError, withExceptT)
import           Control.Monad.State.Strict  (execStateT, get, gets)
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
import           System.FilePath             ((<.>), (</>))
import           TextShow                    (showt)
import           Data.Binary                 (encode)

import           Java.ClassPath
import qualified Java.IO
import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                 hiding (locals)
import           JVM.Builder.Monad           (classPath, encodedCodeLength, execGeneratorT)
import           JVM.ClassFile               hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile               as ClassFile
import           JVM.Converter

import           Backend.ILA                 (Alt(..), AltConstructor(..), Binding(..), Datatype(..), getBindingVariables, isDataAlt, isDefaultAlt, isLiteralAlt)
import           Backend.ILB                 hiding (Converter, ConverterState)
import           ExtraDefs                   (zipOverM_, toLazyBytestring)
import           Logger                      (LoggerT, writeLog)
import           NameGenerator
import           Names                       (VariableName(..), convertName)
import qualified Preprocessor.ContainedNames as ContainedNames
import qualified Typechecker.Types           as Types
import           Typechecker.Hardcoded       (builtinFunctions)
import           Backend.CodeGen.Converter
import           Backend.CodeGen.Hooks       (compilerGeneratedHooks)

writeClass :: FilePath -> NamedClass -> IO ()
writeClass directory (NamedClass name c) = B.writeFile (directory </> unpack name <.> "class") (encode c)

freeVariables :: ContainedNames.HasFreeVariables a => VariableName -> [VariableName] -> a -> Converter (S.Set VariableName)
freeVariables name args x = do
    fvs <- liftErrorText $ ContainedNames.getFreeVariables x
    tls <- gets (M.keysSet . topLevelSymbols)
    -- TODO(kc506): remove these when we remove hardcoded stuff
    let bfs = M.keysSet builtinFunctions
    return $ S.difference fvs (S.unions [S.singleton name, S.fromList args, tls, bfs])

-- |`convert` takes a class name, a path to the primitive java classes, a list of ILB bindings, and the renames used for
-- the top-level symbols and produces a class file
convert :: Text -> FilePath -> [Binding Rhs] -> M.Map VariableName VariableName -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
convert cname primitiveClassDir bs topLevelRenamings = do
    writeLog "-----------"
    writeLog "- CodeGen -"
    writeLog "-----------"
    mainClass <- withExceptT (\e -> unlines [e, showt bs]) action
    dataClasses <- undefined --compileDatatypes
    return $ mainClass:dataClasses
    where bindings = jvmSanitises bs
          initialState = ConverterState
            { localVarCounter = 0
            , datatypes = M.fromList [ ("Bool", Datatype "Bool" [] [("False", []), ("True", [])]) ]
            , topLevelSymbols = M.fromSet (const heapObjectClass) $ S.unions $ M.keysSet builtinFunctions:map getBindingVariables bindings
            , topLevelRenames = topLevelRenamings
            , localSymbols = M.empty
            , initialisers = []
            , dynamicMethods = Seq.empty
            , dictionaries = S.empty --S.singleton $ Types.IsInstance "Num" Types.typeInt  --M.keysSet builtinDictionaries
            , classname = toLazyBytestring cname }
          action = do
            compiled <- addBootstrapMethods inner initialState
            return $ NamedClass cname compiled
          processBinding (NonRec v rhs) = void $ compileGlobalClosure v rhs
          processBinding (Rec m)        = mapM_ (\(v,r) -> processBinding $ NonRec v r) (M.toList m)
          inner = do
            loadPrimitiveClasses primitiveClassDir
            compileDictionaries
            mapM_ processBinding bindings
            addMainMethod
            sequence_ compilerGeneratedHooks

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
        pushGlobalSymbol "_main" heapObjectClass
        invokeVirtual heapObject force
        invokeVirtual heapObject toString
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN

-- |Create a new class for each datatype
compileDatatypes :: [Datatype] -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
compileDatatypes ds = do
    writeLog $ "Compiling datatypes: " <> showt ds
    throwError "Need to implement datatype compilation"

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
    let ms = dynamicMethods converterState
    arg2s <- forM (toList ms) $ \name -> do
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


-- |Compiling a global closure results in a static class field with the given name set to either a `Thunk` or a `Function`
compileGlobalClosure :: VariableName -> Rhs -> Converter (ClassFile.NameType Field)
--compileGlobalClosure v (RhsClosure [] body) =
--    -- As this symbol is global, we store it in a static class field
--    makePublicStaticField (convertName v) (ObjectType heapObject) $ \field -> do
--        -- We're compiling a non-function, so we can just make a thunk
--        compileThunk body
--        cname <- gets classname
--        putStaticField cname field
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
--compileLocalClosure v (RhsClosure [] body) = do
--    -- Same as the global closure, but storing in a local variable rather than a static field
--    compileThunk body
--    storeLocal v
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
    case find (con `elem`) $ map (map fst . branches) $ M.elems ds of
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