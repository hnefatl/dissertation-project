{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

module Typechecker.Typechecker where

import           BasicPrelude                hiding (group)
import           Control.Applicative         (Alternative)
import           Control.Monad.Except        (Except, ExceptT, MonadError, catchError, runExceptT, throwError)
import           Control.Monad.Extra         (whenJust)
import           Control.Monad.State.Strict  (MonadState, StateT, gets, modify, runStateT)
import           Data.Default                (Default, def)
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import           Data.Text                   (unpack)
import           Language.Haskell.Syntax     as Syntax
import           TextShow                    (TextShow, showb, showt)
import           TextShow.Instances          ()

import           AlphaEq
import           ExtraDefs
import           Logger
import           NameGenerator
import           Names
import           Preprocessor.ContainedNames (getBoundVariables)
import           Preprocessor.Dependency
import           Typechecker.Hardcoded
import           Typechecker.Simplifier
import           Typechecker.Substitution
import           Typechecker.Typeclasses
import           Typechecker.Types
import           Typechecker.Unifier


-- |Maps globally unique names of functions/variables/data constructors to a type variable representing their type.
type TypeMap = M.Map VariableName TypeVariableName

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState
    { substitutions    :: Substitution
    , variableTypes    :: M.Map VariableName TypeVariableName -- "In progress" variable -> type variable mappings
    , bindings         :: M.Map VariableName QuantifiedType -- "Finished" variable -> fully described quantified type mappings
    , classEnvironment :: ClassEnvironment
    , kinds            :: M.Map TypeVariableName Kind
    , typePredicates   :: M.Map TypeVariableName (S.Set ClassName)
    , variableCounter  :: Int }
    deriving (Eq, Show)

instance Default InferrerState where
    def = InferrerState
            { substitutions = def
            , variableTypes = M.empty
            , bindings = M.empty
            , classEnvironment = M.empty
            , kinds = M.empty
            , typePredicates = M.empty
            , variableCounter = 0 }
instance TextShow InferrerState where
    showb = fromString . show

-- |A TypeInferrer handles mutable state and error reporting
newtype TypeInferrer a = TypeInferrer (ExceptT Text (StateT InferrerState (LoggerT NameGenerator)) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadState InferrerState, MonadError Text, MonadLogger, MonadNameGenerator)

-- |Run type inference, and return the (possible failed) result along with the last state
runTypeInferrer :: TypeInferrer a -> LoggerT NameGenerator (Except Text a, InferrerState)
runTypeInferrer (TypeInferrer x) = do
    (y, s) <- runStateT (runExceptT x) def
    let z = case y of
            Left err -> throwError $ unlines [err, showt s]
            Right w  -> return w
    return (z, s)

evalTypeInferrer :: TypeInferrer a -> ExceptT Text (LoggerT NameGenerator) a
evalTypeInferrer (TypeInferrer x) = do
    (y, s) <- lift $ runStateT (runExceptT x) def
    case y of
        Left err -> throwError $ unlines [err, showt s]
        Right z  -> return z


nameToType :: TypeVariableName -> TypeInferrer Type
nameToType name = TypeVar <$> nameToTypeVariable name
nameToTypeVariable :: TypeVariableName -> TypeInferrer TypeVariable
nameToTypeVariable name = TypeVariable name <$> getTypeVariableKind name

-- |Generate a type variable name and make it refer to the given type.
nameSimpleType :: Type -> TypeInferrer TypeVariableName
nameSimpleType t = do
    name <- freshTypeVarName
    t' <- nameToType name
    unify t' t
    return name

synToQuantType :: HsQualType -> TypeInferrer QuantifiedType
synToQuantType t = do
    ks <- getKinds
    t' <- synToQualType ks t
    let freeVars = S.map (\v -> TypeVariable v KindStar) $ getTypeVars t'
    return $ Quantified freeVars t'

-- |Returns the current substitution in the monad
getSubstitution :: TypeInferrer Substitution
getSubstitution = gets substitutions
composeSubstitution :: Substitution -> TypeInferrer ()
composeSubstitution sub = modify (\s -> s { substitutions = substitutions s `subCompose` sub })
applyCurrentSubstitution :: Substitutable a => a -> TypeInferrer a
applyCurrentSubstitution x = do
    sub <- getSubstitution
    return $ applySub sub x

-- |If the variable's not in the map, default to it being *: we only store interesting kinds for efficiency.
getTypeVariableKind :: TypeVariableName -> TypeInferrer Kind
getTypeVariableKind name = gets (M.findWithDefault KindStar name . kinds)

getClassEnvironment :: TypeInferrer ClassEnvironment
getClassEnvironment = gets classEnvironment

getConstraints :: TypeVariableName -> TypeInferrer (S.Set ClassName)
getConstraints name = gets (M.findWithDefault S.empty name . typePredicates)

addClasses :: ClassEnvironment -> TypeInferrer ()
addClasses env = modify (\s -> s { classEnvironment = M.union env (classEnvironment s) })

addKinds :: M.Map TypeVariableName Kind -> TypeInferrer ()
addKinds ks = modify (\s -> s { kinds = M.union ks (kinds s) })
getKinds :: TypeInferrer (M.Map TypeVariableName Kind)
getKinds = gets kinds

addVariableType :: VariableName -> TypeVariableName -> TypeInferrer ()
addVariableType name t = addVariableTypes (M.singleton name t)
addVariableTypes :: TypeMap -> TypeInferrer ()
addVariableTypes vs = do
    vts <- gets variableTypes
    let inter = M.intersection vs vts
    unless (M.null inter) $ throwError $ "Overwriting variables " <> showt inter
    modify (\s -> s { variableTypes = M.union vs vts })
-- |Given a variable name, get the type variable name that corresponds
getVariableTypeVariable :: VariableName -> TypeInferrer TypeVariableName
getVariableTypeVariable name = do
    x <- gets (M.lookup name . variableTypes) -- Variable's either provided by the variableTypes
    y <- traverse instantiateToVar =<< gets (M.lookup name . bindings) -- Or the bindings
    maybe (throwError $ "Symbol " <> showt name <> " not in environment") return (x <|> y)
getVariableTypeVariableOrAdd :: VariableName -> TypeInferrer TypeVariableName
getVariableTypeVariableOrAdd name = catchError (getVariableTypeVariable name) $ \_ -> do
    tv <- freshTypeVarName
    addVariableType name tv
    return tv

-- |Instantiate a quantified type into a simple type, replacing all universally quantified variables with new
-- type variables and adding the new type constraints to the environment
instantiateToVar :: QuantifiedType -> TypeInferrer TypeVariableName
instantiateToVar qt = do
    v <- freshTypeVarName
    writeLog $ "Instantiating " <> showt v <> " with " <> showt qt
    Qualified quals t <- instantiate qt
    addTypePredicates quals
    vt <- nameToType v
    unify vt t
    return v

instantiate :: (MonadError Text m, MonadNameGenerator m) => QuantifiedType -> m QualifiedType
instantiate qt@(Quantified _ ql) = do
    -- Create a "default" mapping from each type variable in the type to itself
    let identityMap = M.fromList $ map (\x -> (x, TypeVar $ TypeVariable x KindStar)) $ S.toList $ getTypeVars ql
    -- Create a substitution for each quantified variable to a fresh name, using the identity sub as default
    sub <- Substitution <$> (M.union <$> getInstantiatingTypeMap qt <*> pure identityMap)
    return $ applySub sub ql


addTypeConstraint :: TypeVariableName -> ClassName -> TypeInferrer ()
addTypeConstraint varname classname = addTypeConstraints varname (S.singleton classname)
addTypeConstraints:: TypeVariableName -> S.Set ClassName -> TypeInferrer ()
addTypeConstraints name preds = mergeTypeConstraints (M.singleton name preds)
mergeTypeConstraints :: M.Map TypeVariableName (S.Set ClassName) -> TypeInferrer ()
mergeTypeConstraints ps = modify (\s -> s { typePredicates = M.unionWith S.union ps (typePredicates s) })
addTypePredicate :: TypePredicate -> TypeInferrer ()
addTypePredicate (IsInstance classname (TypeVar (TypeVariable name _))) = addTypeConstraint name classname
addTypePredicate (IsInstance classname (TypeCon (TypeConstant name _))) = addTypeConstraint name classname
addTypePredicate (IsInstance _ TypeApp{})                               = throwError "Not implemented, but should be"
addTypePredicates :: S.Set TypePredicate -> TypeInferrer ()
addTypePredicates = mapM_ addTypePredicate

-- |Intended for use in injecting a known named quantified type into the environment
insertQuantifiedType :: VariableName -> QuantifiedType -> TypeInferrer ()
insertQuantifiedType name t = do
    bs <- gets bindings
    whenJust (M.lookup name bs) $
        \t' -> throwError $ "Overwriting binding " <> showt name <> " :: " <> showt t' <> " with " <> showt t
    modify (\s -> s { bindings = M.insert name t bs })

-- |Given a substitution, propagate constraints on the "from" of the substitution to the "to" of the substitution: eg.
-- if we have `Num t1` and `[t2/t1]` we add a constraint `Num t2`, and if we have `instance (Foo a, Bar b) => Baz (Maybe
-- a b)`, `Foo t1` and `[(Maybe t2 t3)/t1]` then we add constraints `Foo t2` and `Bar t3`.
updateTypeConstraints :: Substitution -> TypeInferrer ()
updateTypeConstraints sub@(Substitution mapping) = forM_ (M.toList mapping) (uncurry helper)
    where helper old (TypeVar (TypeVariable new _)) = addTypeConstraints new =<< getConstraints old
          --helper old (TypeCon (TypeConstant new _)) = addTypeConstraints new =<< getConstraints old
          helper old _ = do
            -- TODO(kc506): If we have eg. `Functor (Maybe a)` and `[Maybe a/t0]` we should be able to infer `Functor
            -- t0`
            constraints <- S.toList <$> getConstraints old
            newPredicates <- fmap S.unions $ forM constraints $ \classInstance -> do
                ce <- getClassEnvironment
                -- Reconstruct the type predicate, apply the substitution, find which constraints it implies
                predicate <- applySub sub <$> (IsInstance classInstance <$> nameToType old)
                newPreds <- ifPThenByInstance ce predicate >>= \case
                    Nothing -> throwError $ "No matching instance for " <> showt predicate
                    Just cs -> return cs
                detectInvalidPredicates ce newPreds
                return newPreds
            writeLog $ "New constraints " <> showt newPredicates <> " from substitution " <> showt sub
            addTypePredicates newPredicates

-- |Extend the current substitution with an mgu that unifies the two arguments
unify :: Type -> Type -> TypeInferrer ()
unify t1 t2 = do
    writeLog $ "Unifying " <> showt t1 <> " with " <> showt t2
    currentSub <- getSubstitution
    -- Update the qualifiers
    newSub <- mgu (applySub currentSub t1) (applySub currentSub t2)
    updateTypeConstraints newSub
    composeSubstitution newSub


getTypePredicates :: TypeVariableName -> TypeInferrer (S.Set TypePredicate)
getTypePredicates name = do
    constraints <- getConstraints name
    S.fromList <$> mapM (\classname -> IsInstance classname <$> nameToType name) (S.toList constraints)

getSimpleType :: TypeVariableName -> TypeInferrer Type
getSimpleType name = applyCurrentSubstitution =<< nameToType name
getQualifiedType :: TypeVariableName -> TypeInferrer QualifiedType
getQualifiedType name = do
    ce <- getClassEnvironment
    t <- getSimpleType name
    let typeVars = getTypeVars t
    predicates <- simplify ce =<< S.unions <$> mapM getTypePredicates (S.toList typeVars)
    let qt = Qualified predicates t
    writeLog $ showt name <> " has qualified type " <> showt qt
    return qt
qualToQuant :: Bool -> QualifiedType -> TypeInferrer QuantifiedType
qualToQuant freshen qt = do
    quantifiers <- S.fromList <$> mapM nameToTypeVariable (S.toList $ getTypeVars qt)
    case freshen of
        False -> return $ Quantified quantifiers qt
        True -> do
            qt' <- instantiate $ Quantified quantifiers qt
            quantifiers' <- S.fromList <$> mapM nameToTypeVariable (S.toList $ getTypeVars qt')
            return $ Quantified quantifiers' qt'
getQuantifiedType :: TypeVariableName -> TypeInferrer QuantifiedType
getQuantifiedType name = do
    qual <- getQualifiedType name
    quant <- qualToQuant False qual
    writeLog $ showt name <> " has quantified type " <> showt quant
    return quant
getVariableQuantifiedType :: VariableName -> TypeInferrer QuantifiedType
getVariableQuantifiedType name = getQuantifiedType =<< getVariableTypeVariable name

-- |If the given type variable name refers to a quantified type, instantiate it and return the new type variable name.
-- If the name refers to a non-quantified type, just return the same name
instantiateIfNeeded :: TypeVariableName -> TypeInferrer TypeVariableName
instantiateIfNeeded name = gets (reverseLookup name . variableTypes) >>= \case
        Just varName -> gets (M.lookup varName . bindings) >>= \case
            Just qt -> instantiateToVar qt
            Nothing -> return name
        Nothing -> return name

nullSrcLoc :: Syntax.SrcLoc
nullSrcLoc = SrcLoc "" 0 0

makeExpTypeWrapper :: Syntax.HsExp -> TypeVariableName -> TypeInferrer (Syntax.HsExp, TypeVariableName)
makeExpTypeWrapper e v = do
    t <- nameToType v
    t' <- qualTypeToSyn (Qualified S.empty t)
    return (HsExpTypeSig nullSrcLoc e t', v)

inferLiteral :: Syntax.HsLiteral -> TypeInferrer TypeVariableName
inferLiteral (HsChar _) = nameSimpleType typeChar
inferLiteral (HsString _) = nameSimpleType typeString
inferLiteral (HsInt _) = do
    v <- freshTypeVarName
    addTypeConstraint v (TypeVariableName "Num")
    return v
inferLiteral (HsFrac _) = do
    v <- freshTypeVarName
    addTypeConstraint v (TypeVariableName "Fractional")
    return v
inferLiteral l = throwError $ "Unboxed literals not supported: " <> showt l

-- |Infer the type of an expression and and return a new node in the AST that can be dropped in instead of the given
-- one, which wraps the given node in an explicit type signature (eg. `5` is replaced with `(5 :: Num t2 => t2)`)
inferExpression :: Syntax.HsExp -> TypeInferrer (Syntax.HsExp, TypeVariableName)
inferExpression e@(HsVar name) = do
    v <- getVariableTypeVariable (convertName name)
    t <- instantiateIfNeeded v
    makeExpTypeWrapper e t
inferExpression e@(HsCon name) = do
    v <- getVariableTypeVariable (convertName name)
    t <- instantiateIfNeeded v
    makeExpTypeWrapper e t
inferExpression e@(HsLit literal) = makeExpTypeWrapper e =<< inferLiteral literal
inferExpression (HsParen e) = inferExpression e
inferExpression (HsLambda l pats body) = do
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    argVars <- inferPatterns pats
    argTypes <- mapM nameToType argVars
    (bodyExp, bodyVar) <- inferExpression body
    bodyType <- nameToType bodyVar
    unify (makeFun argTypes bodyType) retType
    makeExpTypeWrapper (HsLambda l pats bodyExp) retVar
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, and instantiate any quantified variables
    (funExp, funVar) <- inferExpression f
    (argExp, argVar) <- inferExpression e
    funType <- nameToType funVar
    argType <- nameToType argVar
    -- Generate a fresh variable for the return type
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    -- Unify `function` with `argument -> returntype` to match up the types.
    unify (makeFun [argType] retType) funType
    makeExpTypeWrapper (HsApp funExp argExp) retVar
inferExpression (HsInfixApp lhs op rhs) = do
    let VariableName name = convertName op
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp (HsVar $ UnQual $ HsIdent $ unpack name) lhs) rhs)
inferExpression (HsTuple exps) = do
    (argExps, argVars) <- mapAndUnzipM inferExpression exps
    argTypes <- mapM nameToType argVars
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    unify retType (makeTuple argTypes)
    makeExpTypeWrapper (HsTuple argExps) retVar
inferExpression (HsList args) = do
    (argExps, argVars) <- mapAndUnzipM inferExpression args
    argTypes <- mapM nameToType argVars
    -- Check each element has the same type
    commonType <- nameToType =<< freshTypeVarName
    mapM_ (unify commonType) argTypes
    v <- freshTypeVarName
    vt <- nameToType v
    unify vt (makeList commonType)
    makeExpTypeWrapper (HsList argExps) v
inferExpression (HsLet decls body) = do
    -- Process the declarations first (bring into scope any variables etc)
    declExps <- inferDeclGroup decls
    (bodyExp, bodyVar) <- inferExpression body
    makeExpTypeWrapper (HsLet declExps bodyExp) bodyVar
inferExpression (HsCase scrut alts) = do
    -- Oh boy. Get head type, check pattern types match, check alt types match, return common alt type?
    (scrut', scrutVar) <- inferExpression scrut
    scrutType <- nameToType scrutVar
    (alts', commonType) <- inferAlts scrutType alts
    return (HsCase scrut' alts', commonType)
inferExpression (HsIf cond e1 e2) = do
    (condExp, condVar) <- inferExpression cond
    condType <- getQuantifiedType condVar
    let expected = Quantified S.empty (Qualified S.empty typeBool)
    unless (condType == expected) $ throwError $ "if condition " <> showt cond <> " doesn't have type bool"
    (e1Exp, e1Var) <- inferExpression e1
    e1Type <- nameToType e1Var
    (e2Exp, e2Var) <- inferExpression e2
    e2Type <- nameToType e2Var
    commonVar <- freshTypeVarName
    commonType <- nameToType commonVar
    unify commonType e1Type
    unify commonType e2Type
    makeExpTypeWrapper (HsIf condExp e1Exp e2Exp) commonVar
inferExpression (HsExpTypeSig l e t) = do
    -- Unify the given type signature with the variable representing the type of the expression
    (taggedExp, expVar) <- inferExpression e
    ks <- getKinds
    Qualified quals t' <- synToQualType ks t
    unify (TypeVar $ TypeVariable expVar KindStar) t'
    addTypePredicates quals
    -- Wrap this type signature in another one - each expression should be tagged with a type sig, this is no different
    makeExpTypeWrapper (HsExpTypeSig l taggedExp t) expVar
inferExpression e = throwError $ "Unsupported expression: " <> showt e


inferPattern :: Syntax.HsPat -> TypeInferrer TypeVariableName
inferPattern (HsPVar name) = getVariableTypeVariableOrAdd (convertName name)
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = freshTypeVarName
inferPattern (HsPAsPat name pat) = do
    t <- nameToType =<< inferPattern pat
    v <- getVariableTypeVariableOrAdd (convertName name)
    vt <- nameToType v
    unify t vt
    return v
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp con pats) = do
    t <- instantiateToVar =<< getVariableQuantifiedType (convertName con)
    conType <- applyCurrentSubstitution =<< nameToType t
    -- Check the data constructor's been fully applied
    let (args, _) = unmakeCon conType
    unless (length args == length pats) $ throwError "Partial application of data constructor in pattern"
    ts <- mapM nameToType =<< inferPatterns pats
    v <- freshTypeVarName
    vt <- nameToType v
    unify (makeFun ts vt) conType
    return v
inferPattern (HsPTuple pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    v <- freshTypeVarName
    vt <- nameToType v
    unify vt (makeTuple pts)
    return v
inferPattern (HsPList pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    -- Check each element has the same type
    commonType <- nameToType =<< freshTypeVarName
    mapM_ (unify commonType) pts
    v <- freshTypeVarName
    vt <- nameToType v
    unify vt (makeList commonType)
    return v
inferPattern p = throwError $ "Unsupported pattern: " <> showt p

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [TypeVariableName]
inferPatterns = mapM inferPattern

inferAlt :: Type -> HsAlt -> TypeInferrer (HsAlt, TypeVariableName)
inferAlt patType (HsAlt loc pat galt wheres) = do
    wheres' <- inferDeclGroup wheres
    patType' <- nameToType =<< inferPattern pat
    unify patType patType'
    (galt', altVar) <- inferGuardedAlts galt
    return (HsAlt loc pat galt' wheres', altVar)
inferAlts :: Type -> [HsAlt] -> TypeInferrer ([HsAlt], TypeVariableName)
inferAlts patType alts = do
    (alts', vs) <- unzip <$> mapM (inferAlt patType) alts
    commonVar <- freshTypeVarName
    commonType <- nameToType commonVar
    mapM_ (unify commonType) =<< mapM nameToType vs
    return (alts', commonVar)

inferGuardedAlts :: HsGuardedAlts -> TypeInferrer (HsGuardedAlts, TypeVariableName)
inferGuardedAlts (HsUnGuardedAlt e) = do
    (e', v) <- inferExpression e
    return (HsUnGuardedAlt e', v)
inferGuardedAlts (HsGuardedAlts guards) = do
    (guards', vs) <- unzip <$> mapM inferGuardedAlt guards
    commonVar <- freshTypeVarName
    commonType <- nameToType commonVar
    mapM_ (unify commonType) =<< mapM nameToType vs
    return (HsGuardedAlts guards', commonVar)

inferGuardedAlt :: HsGuardedAlt -> TypeInferrer (HsGuardedAlt, TypeVariableName)
inferGuardedAlt (HsGuardedAlt loc cond e) = do
    (cond', condVar) <- inferExpression cond
    condType <- getQuantifiedType condVar
    unless (condType == Quantified S.empty (Qualified S.empty typeBool)) $
        throwError $ unlines ["Expression in case guard has non-boolean type:", synPrint cond, showt condType]
    (e', eVar) <- inferExpression e
    return (HsGuardedAlt loc cond' e', eVar)

-- |Infer the type of a branch of an alternative (as used in case statements and partial defns of functions). The
-- returned expression is the given expression wrapped in an explicit type signature
inferAlternative :: [Syntax.HsPat] -> Syntax.HsExp -> TypeInferrer (Syntax.HsExp, TypeVariableName)
inferAlternative pats e = do
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    patTypes <- mapM nameToType =<< inferPatterns pats
    (bodyExp, bodyVar) <- inferExpression e
    bodyType <- nameToType bodyVar
    unify (makeFun patTypes retType) bodyType
    return (bodyExp, retVar)

inferAlternatives :: [([Syntax.HsPat], Syntax.HsExp)] -> TypeInferrer ([Syntax.HsExp], TypeVariableName)
inferAlternatives alts = do
    (altExps, altVars) <- mapAndUnzipM (uncurry inferAlternative) alts
    altTypes <- mapM nameToType altVars
    commonVar <- freshTypeVarName
    commonType <- nameToType commonVar
    mapM_ (unify commonType) altTypes
    return (altExps, commonVar)

inferRhs :: Syntax.HsRhs -> TypeInferrer (Syntax.HsRhs, TypeVariableName)
inferRhs (HsUnGuardedRhs e) = do
    (newExp, var) <- inferExpression e
    return (HsUnGuardedRhs newExp, var)
inferRhs (HsGuardedRhss _) = throwError "Guarded patterns aren't yet supported"

inferDecl :: Syntax.HsDecl -> TypeInferrer Syntax.HsDecl
inferDecl (HsPatBind l pat rhs ds) = do
    writeLog $ "Processing declaration for " <> synPrint pat
    patType <- nameToType =<< inferPattern pat
    (rhsExp, rhsVar) <- inferRhs rhs
    rhsType <- nameToType rhsVar
    unify patType rhsType
    return $ HsPatBind l pat rhsExp ds
inferDecl (HsFunBind matches) = do
    funName <- case matches of
        [] -> throwError "Function binding with no matches"
        HsMatch _ name _ _ _:_ -> return $ convertName name
    (matches', vs) <- unzip <$> mapM inferMatch matches
    commonVar <- getVariableTypeVariableOrAdd funName
    commonType <- nameToType commonVar
    writeLog $ "Function bind " <> showt funName <> ": commonVar = " <> showt commonVar <> " vs = " <> showt vs
    mapM_ (unify commonType) =<< mapM nameToType vs
    return $ HsFunBind matches'
inferDecl d@(HsTypeSig _ names t) = do
    ks <- getKinds
    t' <- synToQualType ks t
    qt <- qualToQuant True t'
    let varNames = map convertName names
    forM_ varNames (\v -> insertQuantifiedType v qt)
    return d
inferDecl d@(HsClassDecl _ ctx name args decls) = case (ctx, args) of
    ([], [arg]) -> do
        let className = convertName name
        writeLog $ unwords ["Processing decl for", showt className, showt arg]
        forM_ decls $ \case
            HsTypeSig _ names (HsQualType quals t) -> do
                let classPred = (UnQual name, [HsTyVar arg])
                    -- Update eg. `a -> a -> a` in class `Num a` to be `Num a => a -> a -> a`
                    varNames = map convertName names
                quantType <- synToQuantType (HsQualType (classPred:quals) t)
                forM_ varNames (\v -> insertQuantifiedType v quantType)
                writeLog $ "Processed " <> showt names <> ", got " <> showt quantType
            _ -> throwError "Non-type signature declaration found in typeclass"
        -- Update our running class environment
        -- TODO(kc506): When we support contexts, update this to include the superclasses
        addClasses $ M.singleton className (Class S.empty S.empty)
        return d
    ([], _) -> throwError "Multiparameter typeclasses not supported"
    (_, _) -> throwError "Contexts not yet supported in typeclasses"
inferDecl (HsDataDecl loc ctx name args decls derivings) = do
    let typeKind = foldr KindFun KindStar $ replicate (length args) KindStar
    addKinds $ M.singleton (convertName name) typeKind
    let resultType = makeSynApp (HsTyCon $ UnQual name) (map HsTyVar args)
    decls' <- mapM (inferConDecl resultType) decls
    return $ HsDataDecl loc ctx name args decls' derivings
inferDecl _ = throwError "Declaration not yet supported"

inferMatch :: Syntax.HsMatch -> TypeInferrer (Syntax.HsMatch, TypeVariableName)
inferMatch (HsMatch loc name args rhs wheres) = do
    writeLog $ "Inferring match " <> convertName name <> " at " <> showt loc
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    argVars <- inferPatterns args
    argTypes <- mapM nameToType argVars
    (rhs', rhsVar) <- inferRhs rhs
    rhsType <- nameToType rhsVar
    unify (makeFun argTypes rhsType) retType
    retInferred <- getQualifiedType retVar
    writeLog $ "Got match type " <> showt retInferred
    return (HsMatch loc name args rhs' wheres, retVar)

inferConDecl :: HsType -> Syntax.HsConDecl -> TypeInferrer Syntax.HsConDecl
inferConDecl conType d@(HsConDecl _ name args) = do
    let argTypes = flip map args $ \case
            HsBangedTy t   -> t
            HsUnBangedTy t -> t
    qt <- synToQuantType $ HsQualType [] (makeSynFun argTypes conType)
    insertQuantifiedType (convertName name) qt
    return d
inferConDecl _ HsRecDecl{} = throwError "Record data declarations not supported in typechecker"

-- |Returns True if the given declaration can contain recursive references with other declarations
needsRecursiveBinding :: MonadError Text m => Syntax.HsDecl -> m Bool
needsRecursiveBinding HsPatBind{}   = return True
needsRecursiveBinding HsFunBind{}   = return True
needsRecursiveBinding HsClassDecl{} = return False
needsRecursiveBinding HsTypeSig{}   = return False
needsRecursiveBinding HsDataDecl{}  = return False
needsRecursiveBinding d             = throwError $ "Unknown decl " <> showt d

inferDecls :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
inferDecls ds = do
    recursiveDecls <- filterM needsRecursiveBinding ds
    names <- getBoundVariables recursiveDecls
    typeVarMapping <- M.fromList <$> mapM (\n -> (n,) <$> freshTypeVarName) (S.toList names)
    writeLog $ "Adding " <> showt typeVarMapping <> " to environment for declaration group"
    addVariableTypes typeVarMapping
    declExps <- mapM inferDecl ds
    -- Update our name -> type mappings
    mapM_ (\name -> insertQuantifiedType name =<< getQuantifiedType (typeVarMapping M.! name)) (S.toList names)
    return declExps

inferDeclGroup :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
inferDeclGroup ds = do
    dependencyGroups <- dependencyOrder ds
    prettyGroups <- mapM (fmap showtSet . getBoundVariables) dependencyGroups
    writeLog $ "Dependency groups: " <> showt prettyGroups
    declExps <- forM dependencyGroups $ \group -> do
        boundNames <- getBoundVariables group
        writeLog $ "Processing binding group " <> showtSet boundNames
        inferDecls group
    return $ concat declExps

-- TODO(kc506): Take advantage of explicit type hints
inferModule :: Syntax.HsModule -> TypeInferrer (Syntax.HsModule, M.Map VariableName QuantifiedType)
inferModule (HsModule a b c d decls) = do
    writeLog "-----------------"
    writeLog "- Type Inferrer -"
    writeLog "-----------------"
    m <- HsModule a b c d <$> inferDeclGroup decls
    writeLog "Inferred module types"
    ts <- getBoundVariableTypes
    m' <- updateModuleTypeTags m
    writeLog "Updated explicit type tags"
    checkModuleExpTypes m'
    writeLog "Checked explicit type tags"
    return (m', ts)

inferModuleWithBuiltins :: Syntax.HsModule -> TypeInferrer (Syntax.HsModule, M.Map VariableName QuantifiedType)
inferModuleWithBuiltins m = do
    addClasses builtinClasses
    addKinds builtinKinds
    forM_ (M.toList builtinConstructors ++ M.toList builtinFunctions) (uncurry insertQuantifiedType)
    inferModule m

-- |Get the types of all decl-bound variables, ie. {f, x} in `f = let x = 1 in \y -> x + y`
getBoundVariableTypes :: TypeInferrer (M.Map VariableName QuantifiedType)
getBoundVariableTypes = gets bindings
-- |Get all the variable types, so {f, x, y} from `f = let x = 1 in \y -> x + y`. The types of non decl-bound variables
-- aren't guaranteed to really make sense (although they should) (ie, not necessarily have all the constraints they
-- should etc).
getAllVariableTypes :: TypeInferrer (M.Map VariableName QuantifiedType)
getAllVariableTypes = do
    binds <- getBoundVariableTypes
    -- Get the qualified types of each unbound but present variable (and give it an empty quantifier set)
    unboundVariables <- M.toList <$> gets variableTypes
    unbound <- forM unboundVariables $ \(v, t) -> (v,) . Quantified S.empty <$> getQualifiedType t
    return $ M.union binds (M.fromList unbound)


-- |After the 1st pass (`inferModule`) which produced an AST with explicit type tags for each expression, we need to
-- perform this 2nd pass to update the type tags from a stand-in variable to the actual type of the expression.
updateModuleTypeTags :: Syntax.HsModule -> TypeInferrer Syntax.HsModule
updateModuleTypeTags (HsModule a b c d decls) = HsModule a b c d <$> updateDeclsTypeTags decls

updateDeclTypeTags :: Syntax.HsDecl -> TypeInferrer Syntax.HsDecl
updateDeclTypeTags (HsPatBind l pat rhs ds) = HsPatBind l pat <$> updateRhsTypeTags rhs <*> pure ds
updateDeclTypeTags (HsFunBind matches)      = HsFunBind <$> mapM updateMatchTypeTags matches
updateDeclTypeTags d@HsTypeSig{}            = return d
updateDeclTypeTags d@HsClassDecl{}          = return d
updateDeclTypeTags d@HsDataDecl{}           = return d
updateDeclTypeTags _                        = throwError "Unsupported declaration when updating type tags"
updateDeclsTypeTags :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
updateDeclsTypeTags = mapM updateDeclTypeTags

updateRhsTypeTags :: Syntax.HsRhs -> TypeInferrer Syntax.HsRhs
updateRhsTypeTags (HsUnGuardedRhs e) = HsUnGuardedRhs <$> updateExpTypeTags e
updateRhsTypeTags (HsGuardedRhss _)  = throwError "Unsupported RHS when updating type tags"

updateMatchTypeTags :: Syntax.HsMatch -> TypeInferrer Syntax.HsMatch
updateMatchTypeTags (HsMatch loc name args rhs wheres) = HsMatch loc name args <$> updateRhsTypeTags rhs <*> updateDeclsTypeTags wheres

updateExpTypeTags :: Syntax.HsExp -> TypeInferrer Syntax.HsExp
updateExpTypeTags (HsExpTypeSig l e t) = HsExpTypeSig l <$> updateExpTypeTags e <*> case t of
    HsQualType [] (HsTyVar tv) -> qualTypeToSyn =<< getQualifiedType (convertName tv)
    qt                         -> return qt -- Unless the type tag is an unqualified single type variable as inserted by the 1st pass, ignore it
updateExpTypeTags (HsInfixApp e1 op e2) = HsInfixApp <$> updateExpTypeTags e1 <*> pure op <*> updateExpTypeTags e2
updateExpTypeTags (HsApp e1 e2) = HsApp <$> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsNegApp e) = HsNegApp <$> updateExpTypeTags e
updateExpTypeTags (HsLambda l ps e) = HsLambda l ps <$> updateExpTypeTags e
updateExpTypeTags (HsIf c e1 e2) = HsIf <$> updateExpTypeTags c <*> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsTuple es) = HsTuple <$> mapM updateExpTypeTags es
updateExpTypeTags (HsList es) = HsList <$> mapM updateExpTypeTags es
updateExpTypeTags e = return e

checkModuleExpTypes :: MonadError Text m => Syntax.HsModule -> m ()
checkModuleExpTypes (HsModule _ _ _ _ ds) = checkDeclsExpTypes ds
checkDeclExpTypes :: MonadError Text m => Syntax.HsDecl -> m ()
checkDeclExpTypes (HsPatBind _ _ rhs ds) = checkRhsExpTypes rhs >> checkDeclsExpTypes ds
checkDeclExpTypes (HsFunBind matches) = mapM_ checkMatchExpTypes matches
checkDeclExpTypes HsTypeSig{}          = return ()
checkDeclExpTypes HsClassDecl{}          = return ()
checkDeclExpTypes HsDataDecl{}           = return ()
checkDeclExpTypes _ = throwError "Unsupported declaration when checking user-defined explicit type signatures."
checkDeclsExpTypes :: MonadError Text m => [Syntax.HsDecl] -> m ()
checkDeclsExpTypes = mapM_ checkDeclExpTypes
checkRhsExpTypes :: MonadError Text m => Syntax.HsRhs -> m ()
checkRhsExpTypes (HsUnGuardedRhs e) = checkExpExpTypes e
checkRhsExpTypes (HsGuardedRhss _) = throwError "Unsupported RHS when checking user-defined explicit type signatures."
checkMatchExpTypes :: MonadError Text m => Syntax.HsMatch -> m ()
checkMatchExpTypes (HsMatch _ _ _ rhs wheres) = checkRhsExpTypes rhs >> checkDeclsExpTypes wheres

checkExpExpTypes :: MonadError Text m => Syntax.HsExp -> m ()
checkExpExpTypes (HsExpTypeSig _ (HsExpTypeSig _ e manualType) autoType)
    | alphaEq manualType autoType = checkExpExpTypes e
    | otherwise = throwError $ "Type mismatch in explicit type annotation: tagged with " <> x <> " but inferred " <> y
        where x = synPrint manualType
              y = synPrint autoType
checkExpExpTypes (HsInfixApp e1 _ e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsApp e1 e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsNegApp e) = checkExpExpTypes e
checkExpExpTypes (HsLambda _ _ e) = checkExpExpTypes e
checkExpExpTypes (HsIf c e1 e2) = checkExpExpTypes c >> checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsTuple es) = mapM_ checkExpExpTypes es
checkExpExpTypes (HsList es) = mapM_ checkExpExpTypes es
checkExpExpTypes _ = return ()
