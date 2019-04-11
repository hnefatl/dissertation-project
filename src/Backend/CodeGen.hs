{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Backend.CodeGen where

import           BasicPrelude                   hiding (encodeUtf8, head, init)
import           Control.Monad.Except           (ExceptT, runExcept, runExceptT, throwError, withExceptT)
import           Control.Monad.State.Strict     (evalStateT, execStateT, get, gets)
import           Data.Binary                    (encode)
import qualified Data.ByteString.Lazy           as B
import           Data.Foldable                  (null, toList)
import           Data.Functor                   (void)
import qualified Data.Map.Strict                as M
import           Data.Maybe                     (fromJust)
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import           Data.Text                      (pack, unpack)
import           Data.Text.Lazy                 (fromStrict)
import           Data.Text.Lazy.Encoding        (encodeUtf8)
import           Data.Word                      (Word16)
import           System.FilePath                ((<.>), (</>))
import           TextShow                       (TextShow, showt)

import           Java.ClassPath
import qualified Java.IO
import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                    hiding (locals)
import           JVM.ClassFile                  hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile                  as ClassFile
import           JVM.Converter

import           Backend.CodeGen.Converter
import           Backend.CodeGen.Hooks          (compilerGeneratedHooks, primitiveTypes)
import           Backend.CodeGen.JVMSanitisable (jvmSanitise, jvmSanitises)
import           Backend.ILA                    (Alt(..), AltConstructor(..), Binding(..), Datatype(..),
                                                 getBindingVariables, getBranchNames, getConstructorVariables,
                                                 isDataAlt, isDefaultAlt)
import           Backend.ILB
import           ExtraDefs                      (toLazyByteString, zipOverM_)
import           Logger                         (LoggerT, writeLog)
import           NameGenerator
import           Names                          (TypeVariableName, VariableName(..), convertName)
import qualified Preprocessor.ContainedNames    as ContainedNames
import qualified Typechecker.Types              as Types

writeClass :: FilePath -> NamedClass -> IO ()
writeClass directory (NamedClass name c) = B.writeFile (directory </> unpack name <.> "class") (encode c)

freeVariables :: ContainedNames.HasFreeVariables a => VariableName -> [VariableName] -> a -> Converter (S.Set VariableName)
freeVariables name args x = do
    fvs <- liftErrorText $ ContainedNames.getFreeVariables x
    tls <- gets (M.keysSet . topLevelSymbols)
    return $ S.difference fvs (S.unions [S.singleton name, S.fromList args, tls])

-- |`convert` takes a class name, a path to the primitive java classes, a list of ILB bindings, and the renames used for
-- the top-level symbols and produces class files
convert :: Text -> Text -> FilePath -> [Binding Rhs] -> VariableName -> M.Map VariableName VariableName -> M.Map VariableName VariableName -> M.Map TypeVariableName Datatype -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
convert cname packageName primitiveClassDir bs main revRenames topRenames ds = do
    classpath <- loadPrimitiveClasses primitiveClassDir
    let ds' = jvmSanitises ds
        bindings = jvmSanitises bs
        hooks = compilerGeneratedHooks topRenames
        topSymbols = M.fromSet (const heapObjectClass) $ S.unions $ M.keys hooks <> map getBindingVariables bindings <> map getBranchNames (M.elems ds')
        initialState = ConverterState
            { mainName = main
            , localVarCounter = 0
            , datatypes = ds'
            , topLevelSymbols = topSymbols
            , topLevelRenames = topRenames
            , reverseRenames = revRenames
            , localSymbols = M.empty
            , initialisers = map (\gen -> gen cname) $ M.elems hooks
            , dynamicMethods = Seq.empty
            , classname = toLazyByteString cname }
        action = do
            compiled <- addBootstrapMethods initialState classpath packageName $ do
                mapM_ processBinding bindings
                addMainMethod
            return $ NamedClass cname compiled
        processBinding (NonRec v rhs) = void $ compileGlobalClosure v rhs
        processBinding (Rec m)        = mapM_ (\(v,r) -> processBinding $ NonRec v r) (M.toList m)
    writeLog "-----------"
    writeLog "- CodeGen -"
    writeLog "-----------"
    writeLog "Renames"
    forM_ (M.toList revRenames) $ \(r, v) -> writeLog $ showt r <> ": " <> showt v
    dataClasses <- compileDatatypes (M.elems ds') classpath packageName
    mainClass <- withExceptT (\e -> unlines [e, showt bs]) action
    return $ mainClass:dataClasses

loadPrimitiveClasses :: MonadIO m => FilePath -> m [Tree CPEntry]
loadPrimitiveClasses dirPath = liftIO $ execClassPath (mapM_ (loadClass . (dirPath </>)) classes)
    where classes = ["HeapObject", "Function"]

addMainMethod :: Converter ()
addMainMethod = do
    let access = [ ACC_PUBLIC, ACC_STATIC ]
    void $ newMethod access "main" [arrayOf Java.Lang.stringClass] ReturnsVoid $ do
        -- Perform any initialisation actions
        performInitialisers
        getStaticField Java.Lang.system Java.IO.out
        main <- jvmSanitise <$> gets mainName
        pushGlobalSymbol main heapObjectClass
        invokeVirtual heapObject force
        makeUnboxedString -- Convert the result haskell string into a java string
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN

-- |Create a new class for each datatype
compileDatatypes :: [Datatype] -> [Tree CPEntry] -> Text -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
compileDatatypes ds classpath packageName = do
    writeLog $ "Compiling datatypes: " <> showt ds
    fmap catMaybes $ forM ds $ \datatype -> do
        let dname = convertName (typeName datatype)
            dclass = toLazyByteString dname
            methFlags = [ ACC_PUBLIC, ACC_STATIC ]
            compileDatatype = do
                -- Create a boring constructor
                void $ newMethod [ACC_PUBLIC] "<init>" [] ReturnsVoid $ do
                    aload_ I0
                    invokeSpecial boxedData Java.Lang.objectInit
                    i0 RETURN
                -- Compile each constructor into a static "maker" method
                zipOverM_ (M.toList $ branches datatype) [0..] $ \(branchName, args) branchTag -> do
                    void $ addToPool (CClass boxedData)
                    let numArgs = length args
                        methName = toLazyByteString $ "_make" <> convertName branchName
                        methArgs = replicate numArgs heapObjectClass
                        methRet = Returns $ ObjectType $ unpack dname
                    newMethod methFlags methName methArgs methRet $ do
                        -- Create a new instance of the class representing this datatype
                        new dclass
                        dup
                        invokeSpecial dclass Java.Lang.objectInit
                        -- Fill out the branch field with eg. 0 for Nothing or 1 for Just.
                        dup
                        pushInt branchTag
                        putField dclass boxedDataBranch
                        -- Create a data array, fill it with the arguments
                        dup
                        pushInt numArgs
                        allocNewArray heapObject
                        forM_ [0..numArgs - 1] $ \i -> do
                            dup
                            pushInt i
                            loadLocal (fromIntegral i)
                            aastore
                        -- Assign it to the datatype's data field
                        putField dclass boxedDataData
                        -- Return the instance of our class, fully initialised
                        i0 ARETURN
        -- Don't codegen this datatype if it's provided by the runtime classes
        if S.member (typeName datatype) primitiveTypes then
            return Nothing
        else case runExcept $ generate classpath (toLazyByteString packageName <> "." <> dclass) compileDatatype of
            Left err -> throwError $ pack $ show err
            Right ((), out) -> do
                let out' = out { superClass = boxedData }
                return $ Just $ NamedClass dname (classDirect2File out')

-- |We use an invokeDynamic instruction to pass functions around in the bytecode. In order to use it, we need to add a
-- bootstrap method for each function we create.
-- This should be called at the end of the generation of a class file
addBootstrapMethods :: ConverterState -> [Tree CPEntry] -> Text -> Converter () -> ExceptT Text (LoggerT (NameGeneratorT IO)) OutputClass
addBootstrapMethods s cp packageName x = runExceptT (generateT cp (toLazyByteString packageName <> "." <> classname s) $ addBootstrapPoolItems x s) >>= \case
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
            , methodAttributes = AR M.empty }
    mfIndex <- addToPool (CMethodHandle InvokeStatic "java/lang/invoke/LambdaMetafactory" metafactoryMethod)
    -- Add some constants to the pool that we'll need later, when we're creating the bootstrap method attribute
    arg1 <- addToPool (CMethodType "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;")
    arg3 <- addToPool (CMethodType "([LHeapObject;[LHeapObject;)LHeapObject;")
    let ms = dynamicMethods converterState
    arg2s <- forM (toList ms) $ \name -> do
        let sig = MethodSignature [arrayOf heapObjectClass, arrayOf heapObjectClass] (Returns heapObjectClass)
            method = ClassFile.Method
                { methodAccessFlags = S.fromList [ ACC_PUBLIC, ACC_STATIC ]
                , methodName = toLazyByteString name
                , methodSignature = sig
                , methodAttributesCount = 0
                , methodAttributes = AR M.empty }
        addToPool (CMethodHandle InvokeStatic (classname s) method)
    bmIndex <- addToPool (CUTF8 "BootstrapMethods")
    return (bmIndex, mfIndex, arg1, arg2s, arg3)


-- |Compiling a global closure results in a static class field with the given name set to either a `Thunk` or a `Function`
compileGlobalClosure :: VariableName -> Rhs -> Converter (ClassFile.NameType Field)
compileGlobalClosure v (RhsClosure args body) = do
    -- We're compiling a global function, so it shouldn't have any non top-level free variables
    fvs <- freeVariables v args body
    tls <- gets (M.keysSet . topLevelSymbols)
    unless (null fvs) $ throwTextError $ unlines ["Top-level function has free variables: " <> showt v, "FVs: " <> showt fvs, "Args: " <> showt args, "Body: " <> showt body, "Top-level symbols:" <> showt tls]
    -- We compile the function to get an implementation and _makeX function (f is a handle on the _makeX function)
    f <- compileFunction v args body
    -- Get a function instance using f, then store it as a static field
    makePublicStaticField (convertName v) (ObjectType heapObject) $ \field -> do
        cname <- gets classname
        invokeStatic cname f
        putStaticField cname field
-- |Compiling a local closure results in local variable bindings to either a `Thunk` or a `Function`.
compileLocalClosure :: VariableName -> Rhs -> Converter LocalVar
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
    let implName = convertName name <> "Impl"
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
        void $ newMethod implAccs (toLazyByteString implName) implArgs (Returns heapObjectClass) $ do
            compileExp body
            i0 ARETURN
    let wrapperName = "_make" <> convertName name
        numFreeVars = S.size freeVars
        arity = length args
    -- The wrapper function to construct an application instance of the function
    compileMakerFunction wrapperName arity numFreeVars implName

compileExp :: Exp -> Converter ()
compileExp (ExpVar v) = pushSymbol v
compileExp (ExpLit l) = pushLit l
compileExp (ExpApp fun args) = do
    pushSymbol fun -- Grab the function
    invokeVirtual heapObject enter -- Make sure we evaluate any thunks
    invokeVirtual heapObject clone -- Copy it so we don't mutate the reference version
    checkCast function -- Cast the returned Object to a Function
    -- Add each argument to the function object
    forM_ args $ \arg -> do
        dup
        pushArg arg
        invokeVirtual function addArgument
    invokeVirtual function enter
compileExp (ExpConApp con args) = do
    ds <- gets datatypes
    datatype <- case find (\d -> S.member con (M.keysSet $ branches d)) ds of
        Nothing -> throwTextError $ "Datatype constructor not found: " <> showt con <> "\n" <> showt ds
        Just d  -> return d
    let cname = convertName (typeName datatype)
        methodname = "_make" <> convertName con
        methodSig = MethodSignature (replicate numArgs heapObjectClass) (Returns $ ObjectType $ unpack cname)
        numArgs = length args
    -- Push all the datatype arguments onto the stack then call the datatype constructor
    forM_ args pushArg
    invokeStatic (toLazyByteString cname) $ NameType (toLazyByteString methodname) methodSig
compileExp (ExpCase head t vs alts) = do
    let headType = case fst $ Types.unmakeApp t of
            Types.TypeCon (Types.TypeConstant "->" _) -> boxedData
            Types.TypeCon (Types.TypeConstant n _)    -> encodeUtf8 $ fromStrict $ convertName $ jvmSanitise n
            _                                         -> boxedData -- If it's not a datatype, pretend it's boxed data
    writeLog $ showt $ Types.unmakeApp t
    compileExp head
    -- Evaluate the expression
    invokeVirtual heapObject enter
    -- Bind each extra variable to the head
    forM_ vs $ \var -> do
        dup -- Copy the head expression
        storeLocal var -- Store it locally
    -- Branch: if the head object is a boxed data, we extract its branch value. Otherwise, we pop it and push 0 instead.
    dup
    instanceOf headType
    i0 $ IF C_EQ 13 -- If instanceof returned 0, the head's not data. Jump to the "else" block
    do -- The "then" block
        checkCast headType
        dup
        getField headType boxedDataBranch
        goto 4 -- Jump past the "else" block
    do -- The "else" block
        -- Not a data type, so we give a branch value of -1: no datatype can have that branch value, so we'll always
        -- take the default branch
        iconst_m1
    -- Compile the actual case expression
    compileCase headType alts
compileExp (ExpLet var rhs body) = do
    -- Compile the bound expression, store it in a local
    void $ compileLocalClosure var rhs
    -- Compile the inner expression
    compileExp body

compileCase :: B.ByteString -> [Alt Exp] -> Converter ()
compileCase headType as = do
    (defaultAlt, otherAlts) <- case partition isDefaultAlt as of
        ([], alt:otherAlts) -> return (alt, otherAlts) -- Select the first given constructor as the default one
        ([defaultAlt], otherAlts) -> return (defaultAlt, otherAlts)
        (defaultAlts, _) -> throwTextError $ unlines ["Expected single default alt in case statement, got " <> showt (length defaultAlts), unlines $ map showt defaultAlts]
    -- altKeys are the branch numbers of each alt's constructor
    (altKeys, alts) <- unzip <$> sortAlts otherAlts
    s <- get
    let unwrap (Converter x) = x
        processAlt (Alt c e) = unwrap $ do
            bindDataVariables headType (getConstructorVariables c)
            compileExp e
        defaultAltGenerator = processAlt defaultAlt
        altGenerators = map processAlt alts
        runner x = evalStateT x s
    Converter $ lookupSwitchGeneral runner defaultAltGenerator (zip altKeys altGenerators)
-- TODO(kc506): Change to `Maybe VariableName` to allow us to compile `Alt`s without storing all their data parameters,
-- only the ones we use.
bindDataVariables :: B.ByteString -> [VariableName] -> Converter ()
bindDataVariables _ [] = pop
bindDataVariables headType vs = do
    -- Replace the head expression with a ref to its data array
    getField headType boxedDataData
    zipOverM_ vs [0..] $ \var index -> do
        -- Store the data at the given index into the given variable
        dup
        pushInt index
        aaload
        storeLocal var
    pop -- Remove the data array, all the data's store in local variables now


-- |Sort Alts into order of compilation, tagging them with the key to use in the switch statement
sortAlts :: TextShow a => [Alt a] -> Converter [(Word32, Alt a)]
sortAlts [] = return []
sortAlts alts@(Alt (DataCon con _) _:_) = do
    unless (all isDataAlt alts) $ throwTextError "Alt mismatch: expected all data alts"
    ds <- gets datatypes
    -- Find the possible constructors by finding a datatype whose possible branches contain the first alt's constructor
    case find (S.member con) $ map (M.keysSet . branches) $ M.elems ds of
        Nothing -> throwTextError $ "Unknown data constructor: " <> showt con <> "\n" <> showt ds
        Just bs -> do
            let getDataCon (Alt (DataCon c _) _) = c
                getDataCon _                     = error "Compiler error: can only have datacons here"
                okay = all ((`S.member` bs) . getDataCon) alts
            unless okay $ throwTextError "Alt mismatch: constructors from different types"
            let alts' = map (\a -> (fromIntegral $ fromJust $ getDataCon a `elemIndex` S.toAscList bs, a)) alts
            return $ sortOn fst alts'
sortAlts alts@(Alt Default _:_) = throwTextError $ unlines ["Can't have Default constructor", showt alts]
