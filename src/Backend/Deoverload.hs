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
import           Data.Default                (Default, def)
import           Data.Foldable               (foldlM, null, toList)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           Data.Text                   (unpack)
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)

import           ExtraDefs                   (pretty, synPrint, zipOverM)
import           Logger
import           NameGenerator               (MonadNameGenerator, NameGenerator, freshTypeVarName, freshVarName)
import           Names                       (TypeVariableName(..), VariableName(..), convertName)
import           Preprocessor.Info           (ClassInfo(..), getClassInfo)
import           Preprocessor.ContainedNames (HasFreeTypeVariables, getFreeTypeVariables)
import           Preprocessor.Renamer        (renameIsolated)
import           TextShowHsSrc               ()
import           Typechecker.Substitution    (Substitution(..), applySub)
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

runDeoverload :: Deoverload a -> M.Map VariableName QuantifiedType -> M.Map TypeVariableName Kind -> ClassEnvironment -> LoggerT NameGenerator (Except Text a, M.Map VariableName QuantifiedType, M.Map TypeVariableName Kind, DeoverloadState)
runDeoverload action ts ks ce = do
    let ds = M.fromList $ concat $ flip map (M.elems ce) $ \(Class _ instances) ->
                flip map (S.toList instances) $ \(Qualified _ c) -> (c, typePredToDictionaryName c)
        Deoverload inner = addDictionaries ds >> addTypes ts >> addKinds ks >> addClassEnvironment ce >> action
    (x, s) <- runStateT (runExceptT inner) def
    let z = case x of
            Left err -> throwError $ unlines [err, pretty s]
            Right y  -> return y
    return (z, types s, kinds s, s)

evalDeoverload :: Deoverload a -> M.Map VariableName QuantifiedType -> M.Map TypeVariableName Kind -> ClassEnvironment -> ExceptT Text (LoggerT NameGenerator) (a, M.Map VariableName QuantifiedType, M.Map TypeVariableName Kind)
evalDeoverload action ts ks ce = do
    (a, b, c, _) <- lift $ runDeoverload action ts ks ce
    a' <- liftEither $ runExcept a
    return (a', b, c)

typePredToDictionaryName :: TypePredicate -> VariableName
typePredToDictionaryName (IsInstance c t) = VariableName $ "d" <> convertName c <> showt t

addTypes :: M.Map VariableName QuantifiedType -> Deoverload ()
addTypes ts = modify (\s -> s { types = M.union ts (types s) })
tryGetType :: VariableName -> Deoverload (Maybe QuantifiedType)
tryGetType name = gets (M.lookup name . types)
getType :: VariableName -> Deoverload QuantifiedType
getType name = tryGetType name >>= \case
    Nothing ->
        throwError $ "Variable " <> showt name <> " not found in environment"
    Just qt -> return qt

addKinds :: M.Map TypeVariableName Kind -> Deoverload ()
addKinds ks = modify (\s -> s { kinds = M.union ks (kinds s) })
getKinds :: Deoverload (M.Map TypeVariableName Kind)
getKinds = gets kinds

addClassEnvironment :: ClassEnvironment -> Deoverload ()
addClassEnvironment ce = modify (\s -> s { classEnvironment = M.union ce (classEnvironment s) })
getClassEnvironment :: Deoverload ClassEnvironment
getClassEnvironment = gets classEnvironment

addDictionaries :: M.Map TypePredicate VariableName -> Deoverload ()
addDictionaries dicts = do
    existingDicts <- gets dictionaries
    let intersection = M.intersection dicts existingDicts
    unless (null intersection) $ throwError $ "Clashing dictionary types: " <> showt intersection
    -- Generate a type for each new dictionary bound
    let newTypes = M.fromList $ map (\(p, v) -> (v, Quantified S.empty $ Qualified S.empty $ deoverloadTypePredicate p)) (M.toList dicts)
    modify $ \s -> s
        { dictionaries = M.union existingDicts dicts
        , types = M.union newTypes (types s) }
addScopedDictionaries :: M.Map TypePredicate VariableName -> Deoverload a -> Deoverload a
addScopedDictionaries dicts action = do
    -- Add dictionaries, run the action, remove them
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

makeDictName :: MonadNameGenerator m => TypePredicate -> m VariableName
makeDictName (IsInstance (TypeVariableName cl) t) = do
    TypeVariableName suffix <- case t of
        TypeVar (TypeVariable tvn _) -> return tvn
        TypeCon (TypeConstant tcn _) -> return tcn
        TypeApp{}                    -> freshTypeVarName
    return $ VariableName $ "d" <> cl <> suffix

quantifyType :: HasFreeTypeVariables a => a -> Deoverload (Quantified a)
quantifyType t = do
    let frees = getFreeTypeVariables t
    ks <- getKinds
    frees' <- fmap S.fromList $ forM (S.toList frees) $ \n -> return $ TypeVariable n (M.findWithDefault KindStar n ks)
    return $ Quantified frees' t

-- TODO(kc506): Dependency order: we need to process class/data/instance declarations before function definitions.
-- Can wait until we properly support data declarations, as until then we're injecting the class/instance defns manually
deoverloadModule :: HsModule -> Deoverload HsModule
deoverloadModule m@(HsModule a b c d decls) = do
    writeLog "----------------"
    writeLog "- Deoverloader -"
    writeLog "----------------"
    let ci = getClassInfo m
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
    methods <- fmap (sortOn fst) $ concatForM ds $ \decl -> case decl of
        HsTypeSig _ names (HsQualType [] t) -> return [ (name, t) | name <- names ]
        HsTypeSig{} -> throwError $ "No support for class methods with constraints: " <> synPrint decl
        _ -> return []
    let numMethods = length methods
        dataArgs = map (HsBangedTy . snd) methods
        -- dataDecl is `data Foo a = Foo (a -> a) (a -> Int)` for `class Foo a { f :: a -> a ; g :: a -> Int }`
        dataDecl = HsDataDecl nullSrcLoc [] cname args [ HsConDecl nullSrcLoc cname dataArgs ] []
    writeLog $ "Generated data declaration " <> synPrint dataDecl
    -- methodDecls are `f (Foo f' _) = f'` and `g (Foo _ g') = g'` for each method `f`/`g` of the class
    methodDecls <- zipOverM methods [0..] $ \(name, t) i -> do
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
    let conName = convertName cname
        argNames = map convertName args
        conKind = foldr KindFun KindStar (replicate numMethods KindStar)
    writeLog $ "Added kind " <> showt conKind <> " for constructor " <> showt conName
    addKinds $ M.singleton conName conKind
    ks <- getKinds
    argTypes <- mapM (synToType ks . snd) methods
    resultType <- makeApp (TypeCon $ TypeConstant conName conKind) (map (\a -> TypeCon $ TypeConstant a KindStar) argNames)
    let conType = makeFun argTypes resultType
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
    let predicate = IsInstance (convertName name) paramType
    dictName <- gets (M.lookup predicate . dictionaries) >>= \case
        Nothing -> throwError $ "No dictionary name found for " <> showt predicate
        Just n -> return n
    -- Generate a top-level declaration for each member declaration, bound to a new name
    (methodDecls, declRenames) <- fmap unzip $ forM ds $ \case
        HsPatBind l pat rhs wheres -> do
            (pat', renaming) <- (Deoverload $ lift $ lift $ runExceptT $ renameIsolated pat) >>= \case
                Left err -> throwError err
                Right v -> return v
            d' <- HsPatBind l pat' <$> deoverloadRhs rhs <*> deoverloadDecls wheres
            return (d', renaming)
        HsFunBind matches -> do
            newName <- freshVarName
            let synName = convertName newName
                transform (HsMatch loc fname args rhs wheres) = do
                    match' <- deoverloadMatch (HsMatch loc synName args rhs wheres)
                    return (match', fname)
            (matches', fnames) <- unzip <$> mapM transform matches
            case fnames of
                [] -> throwError $ "No matches in function bind"
                fname:_ -> do
                    unless (all (== fname) fnames) $ throwError $ "Function name mismatch in deoverloader"
                    return (HsFunBind matches', M.singleton (convertName fname) newName)
        d -> throwError $ unlines ["Unexpected declaration in deoverloadDecl:", synPrint d]
    -- Work out what the constructor type is, so we can construct a type-tagged expression applying the constructor to
    -- the member declarations.
    let typeSub = Substitution $ M.fromList $ zip (map convertName $ argVariables ci :: [TypeVariableName]) [paramType]
    Quantified _ t' <- deoverloadQuantType <$> getType (convertName name)
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
    -- Add types for each of the generated decls
    dictType <- TypeApp (TypeCon $ TypeConstant (convertName name) (KindFun KindStar KindStar)) <$> synToType ks arg <*> pure KindStar
    addTypes . M.singleton dictName =<< quantifyType (Qualified S.empty dictType)
    writeLog $ "Added type " <> showt dictType <> " for dictionary " <> showt dictName
    forM_ (M.toList $ methods ci) $ \(n, t) -> do
        methodType <- quantifyType . applySub typeSub =<< synToQualType ks t
        case M.lookup (convertName n) renamings of
            Nothing -> throwError $ unwords
                ["Instance declaration for", synPrint name, synPrint arg, "missing definition of", convertName n]
            Just methodName -> do
                addTypes $ M.singleton methodName methodType
                writeLog $ "Added type " <> showt methodType <> " for method " <> showt methodName
    return $ dictDecl:methodDecls
deoverloadDecl d = throwError $ unlines ["Unsupported declaration in deoverloader:", synPrint d]

deoverloadMatch :: HsMatch -> Deoverload HsMatch
deoverloadMatch (HsMatch loc name pats rhs wheres) = do
    -- Replace each constraint with a lambda for a dictionary
    Quantified _ (Qualified quals _) <- getType (convertName name)
    let constraints = S.toAscList quals
    args <- mapM makeDictName constraints
    let dictArgs = M.fromList $ zip constraints args
        funArgs = [ HsPVar $ HsIdent $ unpack arg | VariableName arg <- args ]
    writeLog $ "Processing match " <> showt name <> " " <> showt pats
    (wheres', rhs') <- addScopedDictionaries dictArgs $ (,) <$> deoverloadDecls wheres <*> deoverloadRhsWithoutAddingDicts rhs
    writeLog $ "Finish processing. New args: " <> showt (funArgs <> pats)
    return $ HsMatch loc name (funArgs <> pats) rhs' wheres'

isTypeSig :: HsDecl -> Bool
isTypeSig HsTypeSig{} = True
isTypeSig _           = False

deoverloadRhsWithoutAddingDicts :: HsRhs -> Deoverload HsRhs
deoverloadRhsWithoutAddingDicts (HsUnGuardedRhs expr) = HsUnGuardedRhs <$> deoverloadExp expr
deoverloadRhsWithoutAddingDicts _ = throwError "Unsupported RHS in deoverloader"

deoverloadRhs :: HsRhs -> Deoverload HsRhs
deoverloadRhs rhs@(HsUnGuardedRhs expr) = writeLog ("Deoverloading " <> synPrint expr) >> case expr of
    (HsExpTypeSig loc _ t@(HsQualType _ simpleType)) -> do
        -- Replace each constraint with a lambda for a dictionary
        ks                <- getKinds
        Qualified quals _ <- synToQualType ks t
        let constraints = S.toAscList quals
        args <- mapM makeDictName constraints
        let funArgs = [ HsPVar $ HsIdent $ unpack arg | VariableName arg <- args ]
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
                writeLog $ showt varQual
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

deoverloadQuantType :: QuantifiedType -> QuantifiedSimpleType
deoverloadQuantType (Quantified qs qt) = Quantified qs (deoverloadQualType qt)
deoverloadQualType :: QualifiedType -> Type
deoverloadQualType (Qualified quals t) = makeFun (map deoverloadTypePredicate $ toList quals) t
deoverloadTypePredicate :: TypePredicate -> Type -- TODO(kc506): This should use `getKinds`
deoverloadTypePredicate (IsInstance c t) = TypeApp base t KindStar
    where base = TypeCon $ TypeConstant c (KindFun KindStar KindStar)

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
