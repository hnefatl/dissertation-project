{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

module Backend.Deoverload where

import           BasicPrelude                hiding (exp)
import           Control.Monad.Except        (Except, ExceptT, MonadError, liftEither, runExcept, runExceptT,
                                              throwError)
import           Control.Monad.Extra         (concatForM, concatMapM)
import           Control.Monad.State.Strict  (MonadState, StateT, gets, modify, runStateT)
import           Data.Composition            ((.:))
import           Data.Default                (Default, def)
import           Data.Foldable               (foldlM, null, toList)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           Data.Text                   (unpack)
import qualified Data.Text                   as T
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)

import           ExtraDefs                   (synPrint, zipOverM)
import           Logger
import           NameGenerator               (MonadNameGenerator, NameGenerator, freshVarName)
import           Names                       (TypeVariableName(..), VariableName(..), convertName)
import           Preprocessor.ContainedNames (HasFreeTypeVariables, getFreeTypeVariables)
import           Preprocessor.Info           (ClassInfo(..))
import           Preprocessor.Renamer        (renameIsolated)
import           TextShowHsSrc               ()
import           Typechecker.Substitution    (Substitution(..), TypeSubstitution, applySub)
import           Typechecker.Typechecker     (nullSrcLoc)
import           Typechecker.Typeclasses     (ClassEnvironment, TypeClass(Class), entails)
import           Typechecker.Types
import           Typechecker.Unifier         (mgu)

newtype Deoverload a = Deoverload (ExceptT Text (StateT DeoverloadState (LoggerT NameGenerator)) a)
    deriving (Functor, Applicative, Monad, MonadState DeoverloadState, MonadError Text, MonadLogger, MonadNameGenerator)

data DeoverloadState = DeoverloadState
    { dictionaries     :: M.Map TypePredicate VariableName
    , types            :: M.Map VariableName QuantifiedType
    , kinds            :: M.Map TypeVariableName Kind
    , classEnvironment :: ClassEnvironment
    , classInfo        :: M.Map HsQName ClassInfo }
    deriving (Eq, Show)
instance TextShow DeoverloadState where
    showb = fromString . show
instance Default DeoverloadState where
    def = DeoverloadState
        { dictionaries = M.empty
        , types = M.empty
        , kinds = M.empty
        , classEnvironment = M.empty
        , classInfo = M.empty }

runDeoverload :: Deoverload a -> M.Map VariableName QuantifiedType -> M.Map TypeVariableName Kind -> ClassEnvironment -> LoggerT NameGenerator (Except Text a, DeoverloadState)
runDeoverload action ts ks ce = do
    let ds = M.fromList $ concat $ flip map (M.elems ce) $ \(Class _ instances) ->
                map (\(Qualified _ c) -> (c, makeDictName c)) $ (S.toList instances)
        Deoverload inner = addDictionaries ds >> addTypes ts >> addKinds ks >> addClassEnvironment ce >> action
    (x, s) <- runStateT (runExceptT inner) def
    let z = case x of
            Left err -> throwError err
            Right y  -> return y
    return (z, s)

evalDeoverload :: Deoverload a -> M.Map VariableName QuantifiedType -> M.Map TypeVariableName Kind -> ClassEnvironment -> ExceptT Text (LoggerT NameGenerator) a
evalDeoverload action ts ks ce = do
    (a, _) <- lift $ runDeoverload action ts ks ce
    a' <- liftEither $ runExcept a
    return a'

addTypes :: M.Map VariableName QuantifiedType -> Deoverload ()
addTypes ts = modify (\s -> s { types = M.union ts (types s) })
tryGetType :: VariableName -> Deoverload (Maybe QuantifiedType)
tryGetType name = gets (M.lookup name . types)
getType :: VariableName -> Deoverload QuantifiedType
getType name = tryGetType name >>= \case
    Nothing ->
        throwError $ "Variable " <> showt name <> " not found in environment"
    Just qt -> return qt
addScopedTypes :: M.Map VariableName QuantifiedType -> Deoverload a -> Deoverload a
addScopedTypes ts action = do
    oldTypes <- gets types
    addTypes ts
    res <- action
    modify $ \s -> s { types = oldTypes }
    return res

addKinds :: M.Map TypeVariableName Kind -> Deoverload ()
addKinds ks = modify (\s -> s { kinds = M.union ks (kinds s) })
getKinds :: Deoverload (M.Map TypeVariableName Kind)
getKinds = gets kinds
getKind :: TypeVariableName -> Deoverload Kind
getKind v = gets (M.lookup v . kinds) >>= \case
    Nothing -> throwError $ "No kind found for " <> showt v
    Just k -> return k

addClassEnvironment :: ClassEnvironment -> Deoverload ()
addClassEnvironment ce = modify (\s -> s { classEnvironment = M.union ce (classEnvironment s) })
getClassEnvironment :: Deoverload ClassEnvironment
getClassEnvironment = gets classEnvironment

addDictionaries :: M.Map TypePredicate VariableName -> Deoverload ()
addDictionaries dicts = do
    existingDicts <- gets dictionaries
    let intersection = M.intersection dicts existingDicts
    unless (null intersection) $ throwError $ "Existing dictionary types: " <> showt intersection
    -- Generate a type for each new dictionary bound
    let newTypes = M.fromList $ map (\(p, v) -> (v, Quantified S.empty $ Qualified S.empty $ deoverloadTypePredicate p)) (M.toList dicts)
    modify $ \s -> s
        { dictionaries = M.union existingDicts dicts
        , types = M.union newTypes (types s) }
addScopedDictionaries :: M.Map TypePredicate VariableName -> Deoverload a -> Deoverload a
addScopedDictionaries dicts action = do
    -- Add dictionaries, run the action, remove them. We don't just reset to the old dictionaries in case we added any
    -- new ones while performing the action
    addDictionaries dicts
    result <- action
    modify (\s -> s { dictionaries = M.difference (dictionaries s) dicts })
    return result
getDictionary :: TypePredicate -> Deoverload VariableName
getDictionary p = gets (M.lookup p . dictionaries) >>= \case
    Nothing -> throwError $ "Dictionary " <> showt p <> " not found in environment"
    Just v -> return v
getDictionaryExp :: TypePredicate -> Deoverload HsExp
getDictionaryExp p = do
    VariableName name <- getDictionary p
    return $ HsVar $ UnQual $ HsIdent $ unpack name

makeDictName :: TypePredicate -> VariableName
makeDictName (IsInstance (TypeVariableName cl) t) = VariableName $ "d" <> cl <> suffix
    where suffix = T.filter (/= ' ') $ flattenType t

flattenType :: Type -> Text
flattenType (TypeVar (TypeVariable (TypeVariableName v) _)) = v
-- This might be preventing us from have instances like `instance Show a => Show [a]`, but they're not supported yet
-- anyway
--flattenType TypeVar{} = ""
flattenType (TypeCon (TypeConstant (TypeVariableName c) _)) = c
flattenType (TypeApp t1 t2 _)                               = flattenType t1 <> flattenType t2

quantifyType :: HasFreeTypeVariables a => a -> Deoverload (Quantified a)
quantifyType t = do
    let frees = getFreeTypeVariables t
    ks <- getKinds
    frees' <- fmap S.fromList $ forM (S.toList frees) $ \n -> return $ TypeVariable n (M.findWithDefault KindStar n ks)
    return $ Quantified frees' t

-- TODO(kc506): Dependency order: we need to process class/data/instance declarations before function definitions.
-- Can wait until we properly support data declarations, as until then we're injecting the class/instance defns manually
deoverloadModule :: M.Map HsQName ClassInfo -> HsModule -> Deoverload HsModule
deoverloadModule ci (HsModule a b c d decls) = do
    writeLog "----------------"
    writeLog "- Deoverloader -"
    writeLog "----------------"
    ts <- gets types
    writeLog "Got types:"
    forM_ (M.toList ts) $ \(v,t) -> writeLog $ showt v <> " :: " <> showt t
    modify $ \s -> s { classInfo = ci }
    writeLog $ "Got class information: " <> showt ci
    HsModule a b c d <$> deoverloadDecls decls

deoverloadDecls :: [HsDecl] -> Deoverload [HsDecl]
deoverloadDecls = concatMapM deoverloadDecl

deoverloadDecl :: HsDecl -> Deoverload [HsDecl]
deoverloadDecl (HsPatBind loc pat rhs ds) = pure <$> (HsPatBind loc pat <$> deoverloadRhs rhs <*> pure ds)
deoverloadDecl (HsFunBind matches) = pure <$> (HsFunBind <$> mapM deoverloadMatch matches)
deoverloadDecl (HsTypeSig loc names t) = return [HsTypeSig loc names (HsQualType [] $ deoverloadType t)]
deoverloadDecl (HsClassDecl _ ctx cname args ds) = do
    writeLog $ "Deoverloading class declaration " <> showt cname
    unless (null ctx) $ throwError $ "Class contexts not supported: " <> showt ctx
    -- Sort all type bindings like `x, y :: a -> a` into a list of pairs `[(x, a -> a), (y, a -> a)]`
    -- Convert them to Texts first as otherwise we compare on HsIdent/HsSym etc first before the actual string...
    let toName = (convertName :: HsName -> Text) . fst
    classMethods <- fmap (sortOn toName) $ concatForM ds $ \decl -> case decl of
        HsTypeSig _ names (HsQualType [] t) -> return [ (name, t) | name <- names ]
        HsTypeSig{} -> throwError $ "No support for class methods with constraints: " <> synPrint decl
        _ -> return []
    let numMethods = length classMethods
        dataArgs = map (HsBangedTy . snd) classMethods
        -- dataDecl is `data Foo a = Foo (a -> a) (a -> Int)` for `class Foo a { f :: a -> a ; g :: a -> Int }`
        dataDecl = HsDataDecl nullSrcLoc [] cname args [ HsConDecl nullSrcLoc cname dataArgs ] []
    writeLog $ "Generated data declaration " <> synPrint dataDecl
    -- methodDecls are `f (Foo f' _) = f'` and `g (Foo _ g') = g'` for each method `f`/`g` of the class
    methodDecls <- zipOverM classMethods [0..] $ \(name, t) i -> do
        patVar <- convertName <$> freshVarName
        let pattern = replicate i HsPWildCard ++ [HsPVar patVar] ++ replicate (numMethods - 1 - i) HsPWildCard
            funArgs = [HsPApp (UnQual cname) pattern]
            bodyType = HsQualType [] t
            body = HsExpTypeSig nullSrcLoc (HsVar $ UnQual patVar) bodyType
            dictType = foldl HsTyApp (HsTyCon $ UnQual cname) $ map HsTyVar args
            lamType = HsQualType [] $ makeSynFun [dictType] t
            lam = HsExpTypeSig nullSrcLoc (HsLambda nullSrcLoc funArgs body) lamType
            decl = HsPatBind nullSrcLoc (HsPVar name) (HsUnGuardedRhs lam) []
        writeLog $ "Generated function declaration " <> synPrint decl
        return decl
    -- Add the type and kind of the data constructor to our environment
    ci <- gets (M.lookup (UnQual cname) . classInfo) >>= \case
        Nothing -> throwError $ "No class with name " <> showt cname <> " found."
        Just info -> return info
    let conName = convertName cname
        argNames = map convertName args
        argKinds = map snd $ argVariables ci
        -- The kind of the datatype is a function between the kinds of its arguments
        conKind = foldr KindFun KindStar argKinds
    writeLog $ "Added kind " <> showt conKind <> " for constructor " <> showt conName
    addKinds $ M.singleton conName conKind
    ks <- getKinds
    -- Temporary kinds for the type variables in the class
    let newKinds = M.fromList $ zip argNames argKinds
        ks' = M.union newKinds ks
    argTypes <- mapM (synToType ks' . snd) classMethods
    resultType <- makeApp (TypeCon $ TypeConstant conName conKind) (zipWith (TypeCon .: TypeConstant) argNames argKinds)
    conType <- makeFun argTypes resultType
    conQuantType <- quantifyType $ Qualified S.empty conType
    writeLog $ "Added type " <> showt conQuantType <> " for constructor " <> showt conName
    addTypes $ M.singleton (convertName cname) conQuantType
    return $ dataDecl:methodDecls
deoverloadDecl d@HsDataDecl{} = return [d]
deoverloadDecl (HsInstDecl _ [] name [arg] ds) = do
    ks <- getKinds
    ci <- gets (M.lookup name . classInfo) >>= \case
        Nothing -> throwError $ "No class with name " <> showt name <> " found."
        Just info -> return info
    paramType <- synToType ks arg
    let argVars = argVariables ci
        meths = M.mapKeys convertName $ methods ci
        paramKind = kind paramType
        predicate = IsInstance (convertName name) paramType
        typeSub = Substitution $ M.fromList $ zip (map (convertName . fst) argVars :: [TypeVariableName]) [paramType]
    dictName <- getDictionary predicate

    -- Add the kinds of each of the class variables, so the class' method types have the right kinds
    let argKinds = M.fromList $ map (\(v,k) -> (convertName v,k)) argVars
        ks' = M.union argKinds ks
        addMethodTypes renamings = forM_ (M.toList renamings) $ \(methodName, methodRenamed) ->
            case M.lookup methodName meths of
                Nothing -> throwError $ unwords
                    ["Instance decl for", synPrint name, synPrint arg, "missing definition of", convertName methodName]
                Just t -> do
                    methodType <- quantifyType . applySub typeSub =<< synToQualType ks' t
                    addTypes $ M.singleton methodRenamed methodType
                    writeLog $ "Added type " <> showt methodType <> " for method " <> showt methodRenamed

    -- Generate a top-level declaration for each member declaration, bound to a new name
    (methodDecls, declRenames) <- fmap unzip $ forM ds $ \case
        HsPatBind l pat rhs wheres -> do
            (pat', renaming) <- (Deoverload $ lift $ lift $ runExceptT $ renameIsolated pat) >>= \case
                Left err -> throwError err
                Right v -> return v
            writeLog $ "Pattern " <> synPrint pat <> " " <> showt renaming
            addMethodTypes renaming
            d' <- HsPatBind l pat' <$> deoverloadRhs rhs <*> deoverloadDecls wheres
            return (d', renaming)
        HsFunBind matches -> do
            newName <- freshVarName
            matchName <- case [ matchName | HsMatch _ matchName _ _ _ <- matches ] of
                [] -> throwError $ "No matches in function bind"
                fname:fnames
                    | all (== fname) fnames -> return fname
                    | otherwise -> throwError $ "Function name mismatch in deoverloader"
            let renaming = M.singleton (convertName matchName) newName
            writeLog $ "Method " <> synPrint matchName <> " " <> showt renaming
            addMethodTypes renaming
            let synName = convertName newName
                transform (HsMatch loc _ args rhs wheres) = do
                    fType <- getType newName
                    -- Change the type of this instance of the function to use the instance argument instead of the
                    -- class variable
                    let sub :: TypeSubstitution
                        sub = Substitution $ M.fromList $ zip (map (convertName . fst) argVars) [paramType]
                        fType' = applySub sub fType
                    addScopedTypes (M.singleton newName fType') $
                        deoverloadMatch (HsMatch loc synName args rhs wheres)
            matches' <- mapM transform matches
            return (HsFunBind matches', renaming)
        d -> throwError $ unlines ["Unexpected declaration in deoverloadDecl:", synPrint d]
    -- Work out what the constructor type is, so we can construct a type-tagged expression applying the constructor to
    -- the member declarations.
    Quantified _ t' <- deoverloadQuantType =<< getType (convertName name)
    let t'' = applySub typeSub t'
    conType <- HsQualType [] <$> typeToSyn t''
    writeLog $ "Constructor type: " <> synPrint conType
    let renamings = M.unions declRenames
        conArgs = map (HsVar . UnQual . HsIdent . unpack . convertName . snd) $ M.toAscList renamings
        -- Given a type-tagged constructor and an argument, return a type-tagged application of the arg to the con
        makeConApp :: MonadError Text m => HsExp -> HsExp -> m HsExp
        makeConApp l@(HsExpTypeSig loc _ (HsQualType [] t)) e = do
            (argType, retType) <- unwrapSynFun t
            let r = HsExpTypeSig loc e (HsQualType [] argType)
            return $ HsExpTypeSig loc (HsApp l r) (HsQualType [] retType)
        makeConApp e _ = throwError $ "Illegal expression in makeConApp: " <> showt e
    dictRhs <- HsUnGuardedRhs <$> foldlM makeConApp (HsExpTypeSig nullSrcLoc (HsCon name) conType) conArgs
    let dictDecl = HsPatBind nullSrcLoc (HsPVar $ HsIdent $ unpack $ convertName dictName) dictRhs []
        dictType = TypeApp (TypeCon $ TypeConstant (convertName name) (KindFun paramKind KindStar)) paramType KindStar
    -- Add types for each of the generated decls
    addTypes . M.singleton dictName =<< quantifyType (Qualified S.empty dictType)
    writeLog $ "Added type " <> showt dictType <> " for dictionary " <> showt dictName
    return $ dictDecl:methodDecls
deoverloadDecl d = throwError $ unlines ["Unsupported declaration in deoverloader:", synPrint d]

deoverloadMatch :: HsMatch -> Deoverload HsMatch
deoverloadMatch (HsMatch loc name pats rhs wheres) = do
    -- Replace each constraint with a lambda for a dictionary
    Quantified _ (Qualified quals _) <- getType $ convertName name
    -- Remove any constraints that we need and are already provided by dictionaries
    dicts <- gets dictionaries
    let quals' = S.difference quals (M.keysSet dicts)
        constraints = S.toAscList quals'
        args = map makeDictName constraints
        dictArgs = M.fromList $ zip constraints args
        funArgs = [ HsPVar $ HsIdent $ unpack arg | VariableName arg <- args ]
    writeLog $ "Processing match " <> showt name <> " " <> showt pats
    (wheres', rhs') <- addScopedDictionaries dictArgs $ (,) <$> deoverloadDecls wheres <*> deoverloadRhsWithoutAddingDicts rhs
    writeLog $ "Finished processing. New args: " <> showt (funArgs <> pats)
    return $ HsMatch loc name (funArgs <> pats) rhs' wheres'

isTypeSig :: HsDecl -> Bool
isTypeSig HsTypeSig{} = True
isTypeSig _           = False

deoverloadRhsWithoutAddingDicts :: HsRhs -> Deoverload HsRhs
deoverloadRhsWithoutAddingDicts (HsUnGuardedRhs expr) = HsUnGuardedRhs <$> deoverloadExp expr
deoverloadRhsWithoutAddingDicts _                     = throwError "Unsupported RHS in deoverloader"

deoverloadRhs :: HsRhs -> Deoverload HsRhs
deoverloadRhs rhs@(HsUnGuardedRhs expr) = writeLog ("Deoverloading " <> synPrint expr) >> case expr of
    (HsExpTypeSig loc _ t) -> do
        -- Replace each constraint with a lambda for a dictionary
        ks                <- getKinds
        Qualified quals _ <- synToQualType ks t
        let constraints = S.toAscList quals
            args = map makeDictName constraints
            funArgs = [ HsPVar $ HsIdent $ unpack arg | VariableName arg <- args ]
            dictArgs = M.fromList $ zip constraints args
        e' <- addScopedDictionaries dictArgs (deoverloadRhsWithoutAddingDicts rhs) >>= \case
            HsUnGuardedRhs e' -> return e'
            _ -> undefined
        -- Wrap the expression in a lambda that takes the dictionary arguments
        let outerType = HsQualType [] $ deoverloadType t
            -- The wrapping expression is a lambda taking dictionaries around the inner expression and has modified type
            outerExp = HsExpTypeSig loc (HsLambda loc funArgs e') outerType
        return $ HsUnGuardedRhs $ if null constraints then e' else outerExp
    _ -> throwError $ "Found rhs without top-level type annotation: forgot to run type tagger?\n" <> showt expr
deoverloadRhs _ = throwError "Unsupported RHS in deoverloader"

deoverloadExp :: HsExp -> Deoverload HsExp
deoverloadExp (HsExpTypeSig l (HsVar n) t@(HsQualType _ origSimpleType)) = do
    ks <- getKinds
    let varName = convertName n
    writeLog $  "Deoverloading " <> showt varName <> ", with tagged type " <> synPrint t
    requiredQuals <- tryGetType varName >>= \case
        -- Not a decl-bound name with an original type, so we have to trust the tagged type.
        -- The tagged type includes qualifiers though: eg. `Num a => a` for `x` in `\x -> x + x`, whereas the actual `x`
        -- just has type `a` without the `Num a` qualifier (it's the caller's responsibility to apply any dictionaries
        -- to the arguments). As a result, we don't need any qualifiers, just the simple type.
        Nothing -> do
            writeLog ("Unbound var: " <> showt varName <> " " <> showt origSimpleType)
            return []
        -- The tagged type is "fake": eg. `id :: a -> a` can be tagged with `id :: Num b => b -> b` if `Num b` holds. We
        -- need to make sure we only pass the dictionary arguments that the function/variable we're looking at *needs*.
        -- We get the "real" type of the name, unify the real simple type with the tagged simple type, apply the
        -- substitution to the real type, then see which constraints are actually needed to match the two constraint
        -- sets.
        Just t'@(Quantified _ (Qualified varQuals varSimpleType)) -> do
            writeLog $ "Found bound type " <> showt t'
            Qualified tagQuals tagSimpleType <- synToQualType ks t
            sub <- mgu varSimpleType tagSimpleType
            classEnv <- getClassEnvironment
            -- TODO(kc506): Currently supports finding if the instances of the "fake" type are subclasses of the
            -- "actual" type. To support actually using superclasses, we need to insert applications of functions to
            -- pull out the superclass types though, which means we need to find a path from the subclass to the
            -- superclass and translate it.
            fmap catMaybes $ forM (S.toList $ applySub sub varQuals) $ \varQual -> do
                -- Using ifPThenBySuper would let us check for superclasses. Using entails lets us also grab any
                -- instances like `Num Int` which we'd like to keep so we know to pass a dictionary.
                p <- entails classEnv tagQuals varQual
                return $ if p then Just varQual else Nothing
    writeLog $ "Required qualifiers: " <> showt requiredQuals
    synRequiredQuals <- mapM typePredToSyn requiredQuals
    -- Replace the qualifiers on the tagged type with only the required ones, then deoverload
    let t' = HsQualType [] $ deoverloadType (HsQualType synRequiredQuals origSimpleType)
        e  = HsExpTypeSig l (HsVar n) t'
    -- Apply the variable to any dictionaries it needs
    addDictArgs e =<< mapM getDictionaryExp requiredQuals
-- We don't need to deoverload the constructor name, as constructors can't be qualified in Haskell 98.
deoverloadExp c@(HsCon _) = return c
deoverloadExp (HsExpTypeSig l lit@(HsLit _) (HsQualType _ t)) = return $ HsExpTypeSig l lit (HsQualType [] t)
deoverloadExp (HsExpTypeSig l (HsApp f e) _) = deoverloadExp f >>= \case
    fExp@(HsExpTypeSig _ _ t) -> case t of
        HsQualType [] (HsTyFun _ retType) -> do
            eExp <- deoverloadExp e
            return $ HsExpTypeSig l (HsApp fExp eExp) (HsQualType [] retType)
        _ -> throwError $ "Got non-function type in application: " <> showt t
    _ -> throwError "Got non-tagged function in application"
deoverloadExp HsInfixApp{} = throwError "Infix applications should have been replaced by the type inferrer"
deoverloadExp (HsLambda a pats e) = HsLambda a pats <$> deoverloadExp e
deoverloadExp (HsIf c e1 e2) = HsIf <$> deoverloadExp c <*> deoverloadExp e1 <*> deoverloadExp e2
deoverloadExp (HsTuple es) = HsTuple <$> mapM deoverloadExp es
deoverloadExp (HsList es) = HsList <$> mapM deoverloadExp es
deoverloadExp (HsParen e) = HsParen <$> deoverloadExp e
deoverloadExp (HsLet ds e) = HsLet <$> deoverloadDecls ds <*> deoverloadExp e
deoverloadExp (HsCase scrut alts) = HsCase <$> deoverloadExp scrut <*> mapM deoverloadAlt alts
-- We can ignore any qualifiers: any expression needing them further down the tree will have had the dictionaries passed
-- appropriately and now just have the simple type: we should propagate this change up the tree.
deoverloadExp (HsExpTypeSig l e (HsQualType _ t)) = HsExpTypeSig l <$> deoverloadExp e <*> pure (HsQualType [] t)
deoverloadExp e = throwError $ "Unsupported expression in deoverloader: " <> showt e

deoverloadAlt :: HsAlt -> Deoverload HsAlt
deoverloadAlt (HsAlt loc pat alts wheres) = HsAlt loc pat <$> deoverloadGuardedAlts alts <*> deoverloadDecls wheres

deoverloadGuardedAlts :: HsGuardedAlts -> Deoverload HsGuardedAlts
deoverloadGuardedAlts (HsUnGuardedAlt e) = HsUnGuardedAlt <$> deoverloadExp e
deoverloadGuardedAlts (HsGuardedAlts as) = HsGuardedAlts <$> mapM deoverloadGuardedAlt as

deoverloadGuardedAlt :: HsGuardedAlt -> Deoverload HsGuardedAlt
deoverloadGuardedAlt (HsGuardedAlt loc cond e) = HsGuardedAlt loc <$> deoverloadExp cond <*> deoverloadExp e

-- |Convert eg. `(Num a, Monoid b) => a -> b -> ()` into `Num a -> Monoid b -> a -> b -> ()` to represent the dicts.
deoverloadType :: HsQualType -> HsType
deoverloadType (HsQualType quals t) = makeSynFun (map deoverloadAsst quals) t
deoverloadAsst :: HsAsst -> HsType
deoverloadAsst (name, args) = foldl' HsTyApp (HsTyCon name) args

deoverloadQuantType :: MonadError Text m => QuantifiedType -> m QuantifiedSimpleType
deoverloadQuantType (Quantified qs qt) = Quantified qs <$> deoverloadQualType qt
deoverloadQualType :: MonadError Text m => QualifiedType -> m Type
deoverloadQualType (Qualified quals t) = makeFun (map deoverloadTypePredicate $ toList quals) t
deoverloadTypePredicate :: TypePredicate -> Type
deoverloadTypePredicate (IsInstance c t) = TypeApp base t KindStar
    where base = TypeCon $ TypeConstant c (KindFun (kind t) KindStar)

-- |Given eg. `(+) :: Num a -> a -> a -> a` and `[dNuma]`, produce a type-annotated tree for the following:
-- `((+) :: Num a -> a -> a -> a) (dNuma :: Num a) :: a -> a -> a`
-- Has to take a deoverloaded type, hence the `HsType`.
addDictArgs :: MonadError Text m => HsExp -> [HsExp] -> m HsExp
addDictArgs e@HsExpTypeSig{} [] = return e
addDictArgs e@(HsExpTypeSig loc _ (HsQualType [] (HsTyFun argType retType))) (arg : args) = addDictArgs a args
  where
    r = HsExpTypeSig loc arg (HsQualType [] argType)
    a = HsExpTypeSig loc (HsApp e r) (HsQualType [] retType)
addDictArgs e _ = throwError $ "Invalid expression in deoverloader: " <> showt e
