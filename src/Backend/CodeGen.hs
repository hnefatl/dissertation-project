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
import           Data.Word                      (Word16)
import           System.FilePath                ((<.>), (</>))
import           TextShow                       (showt)

import           Java.ClassPath
import qualified Java.IO
import qualified Java.Lang
import           JVM.Assembler
import           JVM.Builder                    hiding (locals)
import           JVM.ClassFile                  hiding (Class, Field, Method, toString)
import qualified JVM.ClassFile                  as ClassFile
import           JVM.Converter

import           Backend.CodeGen.Converter
import           Backend.CodeGen.Hooks          (compilerGeneratedHooks)
import           Backend.CodeGen.JVMSanitisable (jvmSanitises)
import           Backend.ILA                    (Alt(..), AltConstructor(..), Binding(..), Datatype(..),
                                                 getBindingVariables, isDataAlt, isDefaultAlt, isLiteralAlt)
import           Backend.ILB                    hiding (Converter, ConverterState)
import           ExtraDefs                      (toLazyBytestring, zipOverM_)
import           Logger                         (LoggerT, writeLog)
import           NameGenerator
import           Names                          (TypeVariableName, VariableName(..), convertName)
import qualified Preprocessor.ContainedNames    as ContainedNames
import           Typechecker.Hardcoded          (builtinFunctions)
import qualified Typechecker.Types              as Types

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
convert :: Text -> FilePath -> [Binding Rhs] -> M.Map VariableName VariableName -> M.Map TypeVariableName Datatype -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
convert cname primitiveClassDir bs topLevelRenamings ds = do
    classpath <- loadPrimitiveClasses primitiveClassDir
    let bindings = jvmSanitises bs
        initialState = ConverterState
            { localVarCounter = 0
            , datatypes = jvmSanitises ds
            , topLevelSymbols = M.fromSet (const heapObjectClass) $ S.unions $ M.keys compilerGeneratedHooks <> map getBindingVariables bindings
            , topLevelRenames = topLevelRenamings
            , localSymbols = M.empty
            , initialisers = map (\gen -> gen cname) $ M.elems compilerGeneratedHooks
            , dynamicMethods = Seq.empty
            , dictionaries = S.empty --S.singleton $ Types.IsInstance "Num" Types.typeInt  --M.keysSet builtinDictionaries
            , classname = toLazyBytestring cname }
        action = do
            compiled <- addBootstrapMethods initialState classpath $ do
                mapM_ processBinding bindings
                addMainMethod
            return $ NamedClass cname compiled
        processBinding (NonRec v rhs) = void $ compileGlobalClosure v rhs
        processBinding (Rec m)        = mapM_ (\(v,r) -> processBinding $ NonRec v r) (M.toList m)
    writeLog "-----------"
    writeLog "- CodeGen -"
    writeLog "-----------"
    dataClasses <- compileDatatypes (M.elems ds) classpath
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
        sequence_ =<< gets initialisers
        getStaticField Java.Lang.system Java.IO.out
        pushGlobalSymbol "_main" heapObjectClass
        invokeVirtual heapObject force
        invokeVirtual heapObject toString
        invokeVirtual Java.IO.printStream Java.IO.println
        i0 RETURN

-- |Create a new class for each datatype
compileDatatypes :: [Datatype] -> [Tree CPEntry] -> ExceptT Text (LoggerT (NameGeneratorT IO)) [NamedClass]
compileDatatypes ds classpath = do
    writeLog $ "Compiling datatypes: " <> showt ds
    forM ds $ \datatype -> do
        let dname = "_" <> convertName (typeName datatype)
            dclass = toLazyBytestring dname
            methFlags = [ ACC_PUBLIC, ACC_STATIC ]
            compileDatatype = zipOverM_ (branches datatype) [0..] $ \(branchName, args) branchTag -> do
                void $ addToPool (CClass boxedData)
                let numArgs = length args
                    methName = toLazyBytestring $ "_make" <> convertName branchName
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
        case runExcept $ generate classpath dclass compileDatatype of
            Left err -> throwError $ pack $ show err
            Right ((), out) -> do
                let out' = out { superClass = boxedData }
                return $ NamedClass dname (classDirect2File out')

-- TODO(kc506): Should be uneccessary: dictionaries should be created by `instance` decls in ILA conversion
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
addBootstrapMethods :: ConverterState -> [Tree CPEntry] -> Converter () -> ExceptT Text (LoggerT (NameGeneratorT IO)) OutputClass
addBootstrapMethods s cp x = runExceptT (generateT cp (classname s) $ addBootstrapPoolItems x s) >>= \case
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
    let implName = "_" <> convertName name <> "Impl"
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
        void $ newMethod implAccs (toLazyBytestring implName) implArgs (Returns heapObjectClass) $ do
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
    case find (\d -> con `elem` map fst (branches d)) ds of
        Nothing -> throwTextError $ "Datatype constructor not found: " <> showt con <> "\n" <> showt ds
        Just datatype -> do
            let cname = "_" <> convertName (typeName datatype)
                methodname = "_make" <> convertName con
                methodSig = MethodSignature (replicate numArgs heapObjectClass) (Returns $ ObjectType $ unpack cname)
                numArgs = length args
            -- Push all the datatype arguments onto the stack then call the datatype constructor
            forM_ args pushArg
            invokeStatic (toLazyBytestring cname) $ NameType (toLazyBytestring methodname) methodSig
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
compileCase as = case partition isDefaultAlt as of
    ([defaultAlt], otherAlts) -> do
        -- altKeys are the branch numbers of each alt's constructor
        (altKeys, alts) <- unzip <$> sortAlts otherAlts
        s <- get
        let defaultAltGenerator = (\(Alt _ vs e) -> unwrap $ bindDataVariables vs >> compileExp e) defaultAlt
            altGenerators = map (\(Alt _ vs e) -> unwrap $ bindDataVariables vs >> compileExp e) alts
            unwrap (Converter x) = x
            runner x = evalStateT x s
        Converter $ lookupSwitchGeneral runner defaultAltGenerator (zip altKeys altGenerators)
    _ -> throwTextError "Expected single default alt in case statement"

-- TODO(kc506): Change to `Maybe VariableName` to allow us to compile `Alt`s without storing all their data parameters,
-- only the ones we use.
bindDataVariables :: [VariableName] -> Converter ()
bindDataVariables vs = do
    -- Replace the head expression with a ref to its data array
    getField heapObject $ NameType "data" (arrayOf boxedDataClass)
    zipOverM_ vs [0..] $ \var index -> do
        -- Store the data at the given index into the given variable
        dup
        pushInt index
        aaload
        storeLocal var
    pop -- Remove the data array, all the data's store in local variables now


-- |Sort Alts into order of compilation, tagging them with the key to use in the switch statement
sortAlts :: [Alt a] -> Converter [(Word32, Alt a)]
sortAlts [] = return []
sortAlts alts@(Alt (DataCon con) _ _:_) = do
    unless (all isDataAlt alts) $ throwTextError "Alt mismatch: expected all data alts"
    ds <- gets datatypes
    -- Find the possible constructors by finding a datatype whose possible branches contain the first alt's constructor
    case find (con `elem`) $ map (map fst . branches) $ M.elems ds of
        Nothing -> throwTextError $ "Unknown data constructor: " <> showt con <> "\n" <> showt ds
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
