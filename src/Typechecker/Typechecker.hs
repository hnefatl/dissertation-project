{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

module Typechecker.Typechecker where

import           BasicPrelude                hiding (group)
import           Control.Applicative         (Alternative)
import           Control.Monad.Except        (Except, ExceptT, MonadError, catchError, runExceptT, throwError)
import           Control.Monad.State.Strict  (MonadState, StateT, evalStateT, get, gets, modify, put, runStateT)
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
import           Preprocessor.ContainedNames (HasFreeVariables, getBoundVariables, getFreeVariables)
import           Preprocessor.Dependency
import           Preprocessor.Info           (ClassInfo(..), getClassInfo, getModuleKinds)
import           Tuples                      (makeTupleName)
import           Typechecker.Hardcoded
import           Typechecker.Simplifier
import           Typechecker.Substitution
import           Typechecker.Typeclasses     hiding (addClass, addInstance)
import qualified Typechecker.Typeclasses     as TypeClasses
import           Typechecker.Types
import           Typechecker.Unifier


-- |Maps globally unique names of functions/variables/data constructors to a type variable representing their type.
type TypeMap = M.Map VariableName TypeVariableName

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState
    { substitutions      :: TypeSubstitution
    -- "In progress" variable -> type variable mappings
    , variableTypes      :: M.Map VariableName TypeVariableName
    -- "Finished" variable -> fully described quantified type mappings
    , bindings           :: M.Map VariableName QuantifiedType
    , classEnvironment   :: ClassEnvironment
    , kinds              :: M.Map TypeVariableName Kind
    -- Type predicates: this is not redundant given `classEnvironment`, this contains eg. "type variable t74 is
    -- constrained by these classes", whereas `classEnvironment` contains eg. "Bool" is an instance of "Monoid".
    , typePredicates     :: M.Map Type (S.Set ClassName)
    -- Any typeclass instances we find when typechecking: we process these either after all the other decls or when
    -- another declaration needs a typeclass instance we can provide by processing one of these. It's weird, but
    -- necessary as other decls depend on them at the type level instead of at the syntactic level, which we can't
    -- detect, so we just process them as late as possible.
    , typeclassInstances :: M.Map (HsQName, [HsType]) HsDecl
    , classInfo          :: M.Map HsQName ClassInfo }
    deriving (Eq, Show)

instance Default InferrerState where
    def = InferrerState
            { substitutions = def
            , variableTypes = M.empty
            , bindings = M.empty
            , classEnvironment = M.empty
            , kinds = M.empty
            , typePredicates = M.empty
            , typeclassInstances = M.empty
            , classInfo = M.empty }
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
            Left err -> throwError err
            Right w  -> return w
    return (z, s)

evalTypeInferrer :: TypeInferrer a -> ExceptT Text (LoggerT NameGenerator) a
evalTypeInferrer (TypeInferrer x) = do
    y <- lift $ evalStateT (runExceptT x) def
    case y of
        Left err -> throwError err
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
    let freeVars = getTypeVars t'
    return $ Quantified freeVars t'

-- |Returns the current substitution in the monad
getSubstitution :: TypeInferrer TypeSubstitution
getSubstitution = gets substitutions
composeSubstitution :: TypeSubstitution -> TypeInferrer ()
composeSubstitution sub = modify (\s -> s { substitutions = substitutions s `subCompose` sub })
applyCurrentSubstitution :: TypeSubstitutable a => a -> TypeInferrer a
applyCurrentSubstitution x = do
    sub <- getSubstitution
    return $ applySub sub x

-- |If the variable's not in the map, default to it being *: we only store interesting kinds for efficiency.
getTypeVariableKind :: TypeVariableName -> TypeInferrer Kind
getTypeVariableKind name = gets (M.findWithDefault KindStar name . kinds)

getClassEnvironment :: TypeInferrer ClassEnvironment
getClassEnvironment = gets classEnvironment

getConstraints :: Type -> TypeInferrer (S.Set ClassName)
getConstraints name = gets (M.findWithDefault S.empty name . typePredicates)

setClasses :: ClassEnvironment -> TypeInferrer ()
setClasses env = modify (\s -> s { classEnvironment = env })
addClass :: ClassName -> S.Set ClassName -> TypeInferrer()
addClass name supers = do
    ce <- gets classEnvironment
    ce' <- TypeClasses.addClass name supers ce
    modify (\s -> s { classEnvironment = ce' })

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

instantiate :: (TypeSubstitutable t, MonadNameGenerator m) => Quantified t -> m t
instantiate (Quantified quants t) = do
    let giveNewName (TypeVariable oldName k) = do
            newName <- freshTypeVarName
            return (oldName, TypeVar $ TypeVariable newName k)
    sub <- Substitution . M.fromList <$> mapM giveNewName (S.toList quants)
    return $ applySub sub t


addTypeConstraint :: Type -> ClassName -> TypeInferrer ()
addTypeConstraint t classname = addTypeConstraints t (S.singleton classname)
addTypeConstraints:: Type -> S.Set ClassName -> TypeInferrer ()
addTypeConstraints t preds = mergeTypeConstraints (M.singleton t preds)
mergeTypeConstraints :: M.Map Type (S.Set ClassName) -> TypeInferrer ()
mergeTypeConstraints ps = modify $ \s -> s { typePredicates = M.unionWith S.union ps $ typePredicates s }
addTypePredicate :: TypePredicate -> TypeInferrer ()
addTypePredicate (IsInstance classname t) = addTypeConstraint t classname
addTypePredicates :: S.Set TypePredicate -> TypeInferrer ()
addTypePredicates = mapM_ addTypePredicate

-- |Intended for use in injecting a known named quantified type into the environment
insertQuantifiedType :: VariableName -> QuantifiedType -> TypeInferrer ()
insertQuantifiedType name t = do
    bs <- gets bindings
    writeLog $ "Inserting qualified type " <> showt name <> " :: " <> showt t
    t' <- case M.lookup name bs of
        Nothing -> return t -- This is the only type we have for this symbol
        Just existingType -> do -- We've got an existing type: if we can match the types, we're good
            existingQualType <- instantiate existingType
            qualT <- instantiate t
            maybeSub <- tryM (mgu qualT existingQualType)
            case maybeSub of
                Right sub -> qualToQuant True S.empty $ applySub sub existingQualType
                Left err -> throwError $ unlines
                    [ "Can't match existing type with new type for " <> showt name
                    , "Existing: " <> showt existingQualType
                    , "New: " <> showt qualT
                    , "Error: " <> err ]
    modify (\s -> s { bindings = M.insert name t' bs })

-- |Given a substitution, propagate constraints on the "from" of the substitution to the "to" of the substitution: eg.
-- if we have `Num t1` and `[t2/t1]` we add a constraint `Num t2`, and if we have `instance (Foo a, Bar b) => Baz (Maybe
-- a b)`, `Foo t1` and `[(Maybe t2 t3)/t1]` then we add constraints `Foo t2` and `Bar t3`.
updateTypeConstraints :: TypeSubstitution -> TypeInferrer ()
updateTypeConstraints sub@(Substitution mapping) = forM_ (M.toList mapping) (uncurry helper)
    where helper :: TypeVariableName -> Type -> TypeInferrer ()
          -- Easy to transfer constraints from a variable to a variable: just copy them over
          helper old t@(TypeVar TypeVariable{}) = addTypeConstraints t =<< getConstraints =<< nameToType old
          -- Harder to transfer constraints from a variable to a type
          helper old t = do
            -- TODO(kc506): If we have eg. `Functor (Maybe a)` and `[Maybe a/t0]` we should be able to infer `Functor
            -- t0`
            constraints <- S.toList <$> (getConstraints =<< nameToType old)
            newPredicates <- fmap S.unions $ forM constraints $ \classInstance -> do
                ce <- getClassEnvironment
                -- Reconstruct the type predicate, apply the substitution, find which constraints it implies
                predicate <- applySub sub <$> (IsInstance classInstance <$> nameToType old)
                newPreds <- ifPThenByInstance ce predicate >>= \case
                    Nothing -> do
                        writeLog "Not found"
                        -- Failed to find an instance: check if we can create one on-demand from the typeclass instance
                        -- declarations we've not yet processed
                        synType <- typeToSyn =<< applyCurrentSubstitution t
                        let key = (UnQual $ HsIdent $ unpack $ convertName classInstance, [synType])
                        insts <- gets typeclassInstances
                        case M.lookup key insts of
                            Just d -> do
                                -- We've got a typeclass declaration for the instance we want: remove it, process it,
                                -- then retry
                                modify $ \s -> s { typeclassInstances = M.delete key (typeclassInstances s) }
                                addTypeclassInstanceFromDecl d
                                ce' <- getClassEnvironment
                                ifPThenByInstance ce' predicate >>= \case
                                    Just cs -> return cs -- Cool, the declaration worked
                                    Nothing -> throwError $ "No matching instance for " <> showt predicate
                            Nothing -> throwError $ "No typeclass declaration found that provides an instance for " <> showt predicate
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

-- |Like unify but only updates variables in the left argument
unifyLeft :: Type -> Type -> TypeInferrer ()
unifyLeft t1 t2 = do
    writeLog $ "Matching " <> showt t1 <> " with " <> showt t2
    currentSub <- getSubstitution
    -- Update the qualifiers
    newSub <- match (applySub currentSub t1) (applySub currentSub t2)
    updateTypeConstraints newSub
    composeSubstitution newSub


getTypePredicates :: Type -> TypeInferrer (S.Set TypePredicate)
getTypePredicates t = do
    constraints <- getConstraints t
    return $ S.map (\classname -> IsInstance classname t) constraints

getSimpleType :: TypeVariableName -> TypeInferrer Type
getSimpleType name = applyCurrentSubstitution =<< nameToType name
getQualifiedType :: TypeVariableName -> TypeInferrer QualifiedType
getQualifiedType name = do
    ce <- getClassEnvironment
    t <- getSimpleType name
    let types = map TypeVar (S.toList $ getTypeVars t)
    predicates <- simplify ce =<< S.unions <$> mapM getTypePredicates types
    let qt = Qualified predicates t
    --writeLog $ showt name <> " has qualified type " <> showt qt
    return qt
qualToQuant :: Bool -> S.Set TypeVariableName -> QualifiedType -> TypeInferrer QuantifiedType
qualToQuant freshen freeTvs qt = do
    -- Update the type variables to their latest values
    freeTvs' <- (S.fromList . catMaybes <$>) $ forM (S.toList freeTvs) $ \tv -> do
        t <- nameToType tv
        t' <- applyCurrentSubstitution t
        return $ case t' of
            TypeVar (TypeVariable v _) -> Just v
            _                          -> Nothing
    let filterFreeTvs = S.filter (\(TypeVariable v _) -> S.notMember v freeTvs')
        quantifiers = filterFreeTvs $ getTypeVars qt
    case freshen of
        False -> return $ Quantified quantifiers qt
        True -> do
            qt' <- instantiate $ Quantified quantifiers qt
            let quantifiers' = filterFreeTvs $ getTypeVars qt'
            return $ Quantified quantifiers' qt'
getQuantifiedType :: TypeVariableName -> S.Set TypeVariableName -> TypeInferrer QuantifiedType
getQuantifiedType name freeTvs = do
    qual <- getQualifiedType name
    quant <- qualToQuant False freeTvs qual
    writeLog $ showt name <> " has quantified type " <> showt quant
    return quant
getVariableQuantifiedType :: VariableName -> S.Set TypeVariableName -> TypeInferrer QuantifiedType
getVariableQuantifiedType name freeTvs = flip getQuantifiedType freeTvs =<< getVariableTypeVariable name

getFreeTypeVariableNames :: HasFreeVariables a => a -> TypeInferrer (S.Set TypeVariableName)
getFreeTypeVariableNames x = do
    fvs <- S.toList <$> getFreeVariables x
    S.fromList <$> mapM getVariableTypeVariable fvs

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
    addTypeConstraint (TypeVar $ TypeVariable v KindStar) (TypeVariableName "Num")
    addTypeConstraint (TypeVar $ TypeVariable v KindStar) (TypeVariableName "Eq")
    return v
inferLiteral (HsFrac _) = do
    v <- freshTypeVarName
    addTypeConstraint (TypeVar $ TypeVariable v KindStar) (TypeVariableName "Fractional")
    addTypeConstraint (TypeVar $ TypeVariable v KindStar) (TypeVariableName "Eq")
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
    targetType <- makeFun argTypes bodyType
    unify targetType retType
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
    targetType <- makeFun [argType] retType
    unify targetType funType
    makeExpTypeWrapper (HsApp funExp argExp) retVar
inferExpression (HsInfixApp lhs op rhs) = do
    let op' = case op of
            HsQVarOp n -> HsVar n
            HsQConOp n -> HsCon n
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp op' lhs) rhs)
inferExpression (HsTuple es) = do
    -- See the note in HsList below
    let tupleCon = HsCon $ UnQual $ HsSymbol $ unpack $ makeTupleName $ length es
    inferExpression $ foldl' HsApp tupleCon es
inferExpression (HsList es) = do
    -- If the renamer has been run, we should never see a HsList node. The only case we can is if a test is being run
    -- without renaming, so we should use the non-renamed constructor names
    let nilCon = HsCon $ UnQual $ HsSymbol "[]"
        consCon = HsCon $ UnQual $ HsSymbol ":"
    inferExpression $ foldr (HsApp . HsApp consCon) nilCon es
inferExpression (HsLet decls body) = do
    -- Process the declarations first (bring into scope any variables etc)
    declExps <- inferDeclGroup decls
    (bodyExp, bodyVar) <- inferExpression body
    makeExpTypeWrapper (HsLet declExps bodyExp) bodyVar
inferExpression (HsCase scrut alts) = do
    (scrut', scrutVar) <- inferExpression scrut
    scrutType <- nameToType scrutVar
    (alts', commonType) <- inferAlts scrutType alts
    makeExpTypeWrapper (HsCase scrut' alts') commonType
inferExpression (HsIf cond e1 e2) = do
    (condExp, condVar) <- inferExpression cond
    unify typeBool =<< nameToType condVar
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
    unify (TypeVar $ TypeVariable expVar (kind t')) t'
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
inferPattern (HsPInfixApp p1 con p2) = inferPattern $ HsPApp con [p1, p2]
inferPattern (HsPApp con pats) = do
    t <- instantiateToVar =<< getVariableQuantifiedType (convertName con) S.empty
    conType <- applyCurrentSubstitution =<< nameToType t
    -- Check the data constructor's been fully applied
    let (args, _) = unmakeCon conType
    unless (length args == length pats) $ throwError "Partial application of data constructor in pattern"
    ts <- mapM nameToType =<< inferPatterns pats
    v <- freshTypeVarName
    vt <- nameToType v
    targetType <- makeFun ts vt
    unify targetType conType
    return v
inferPattern (HsPTuple pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    v <- freshTypeVarName
    vt <- nameToType v
    unify vt =<< makeTuple pts
    return v
inferPattern (HsPList pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    -- Check each element has the same type
    commonType <- nameToType =<< freshTypeVarName
    mapM_ (unify commonType) pts
    v <- freshTypeVarName
    vt <- nameToType v
    unify vt =<< makeList commonType
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
    unify typeBool =<< nameToType condVar
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
    targetType <- makeFun patTypes retType
    unify targetType bodyType
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
        []                     -> throwError "Function binding with no matches"
        HsMatch _ name _ _ _:_ -> return $ convertName name
    (matches', vs) <- unzip <$> mapM inferMatch matches
    commonVar <- getVariableTypeVariableOrAdd funName
    commonType <- nameToType commonVar
    writeLog $ "Function bind " <> showt funName <> ": commonVar = " <> showt commonVar <> " vs = " <> showt vs
    mapM_ (unify commonType) =<< mapM nameToType vs
    return $ HsFunBind matches'
inferDecl d@(HsClassDecl _ _ name args decls) = do
    let className = convertName name
    writeLog $ unwords ["Processing class decl for", showt className, unwords $ map showt args]
    ci <- gets (M.lookup (UnQual name) . classInfo) >>= \case
        Nothing -> throwError $ "No class with name " <> showt name <> " found."
        Just info -> return info
    let classVariableKinds = M.fromList [ (convertName v, k) | (v, k) <- argVariables ci]
        classKind = foldr KindFun KindStar classVariableKinds
    addKinds $ M.singleton className classKind
    ks <- getKinds
    let ks' = M.union ks classVariableKinds
    forM_ decls $ \case
        HsTypeSig _ names (HsQualType quals t) -> do
            -- Augment the type with the class constraint, eg. `Functor a`
            let classPred = (UnQual name, map HsTyVar args)
                varNames = map convertName names
            qualType <- synToQualType ks' (HsQualType (classPred:quals) t)
            qt <- qualToQuant True S.empty qualType
            forM_ varNames (\v -> writeLog (showt v <> " ::" <> showt qt) >> insertQuantifiedType v qt)
        _ -> throwError "Only support type signature declarations in typeclasses"
    -- Update our running class environment
    -- TODO(kc506): When we support contexts, update this to include the superclasses
    addClass className S.empty
    return d
inferDecl (HsInstDecl loc ctx name args decls) = case (ctx, args) of
    ([], [_]) -> HsInstDecl loc ctx name args <$> checkInstanceDecls name args decls
    ([], _)   -> throwError "Multiparameter typeclass instances not supported"
    (_, _)    -> throwError "Contexts not yet supported in instances"
inferDecl (HsDataDecl loc ctx name args decls derivings) = do
    let resultType = makeSynApp (HsTyCon $ UnQual name) (map HsTyVar args)
    decls' <- mapM (inferConDecl resultType) decls
    return $ HsDataDecl loc ctx name args decls' derivings
inferDecl d@(HsTypeSig _ names t) = do
    ks <- getKinds
    t' <- synToQualType ks t
    forM_ (map convertName names) $ \v -> do
        -- Freshen variables for each bound name so we don't duplicate type variables between names
        qt <- qualToQuant True S.empty t'
        insertQuantifiedType v qt
    return d
inferDecl _ = throwError "Declaration not yet supported"

-- |For typeclass instance "top-level" declarations, we don't need to infer the type: it's easily obtained by
-- substituting the class type variable parameters for the concrete types in the instance head in the type of each
-- binding. We need to process them separately to normal declarations as normal declarations assume they're the only
-- definition of the symbol: typeclass instance definitions might be one of many.
checkInstanceDecls :: HsQName -> [HsType] -> [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
checkInstanceDecls cname argSynTypes ds = do
    writeLog $ "Processing instance declaration for:" <> unlines (map synPrint ds)
    -- When inferring the pattern type, we want to inject the type of the binding given the instance variables: in
    -- `instance Num Int`, `(+)` doesn't have type `Num a => a -> a -> a`, it has type `Int -> Int -> Int`.
    ci <- gets (M.lookup cname . classInfo) >>= \case
        Nothing -> throwError $ "No class with name " <> showt cname <> " found."
        Just info -> return info
    ks <- getKinds
    let classVariableKinds = M.fromList [ (convertName v, k) | (v, k) <- argVariables ci]
        ks' = M.union ks classVariableKinds
    argTypes <- mapM (synToType ks') argSynTypes
    let sub = Substitution $ M.fromList $ zip (map (convertName . fst) $ argVariables ci) argTypes  :: TypeSubstitution
    methodTypes <- fmap M.fromList $ forM (M.toList $ methods ci) $ \(m, t) -> do
        qualType <- synToQualType ks' t
        let qt = applySub sub $ Quantified (getTypeVars qualType) qualType
        t' <- instantiateToVar qt
        return (convertName m, t')
    forM ds $ \d -> case d of
        HsPatBind loc pat rhs wheres -> do
            -- Temporarily add the renamed types, infer the pattern, then reset to the old types
            origTypes <- gets variableTypes
            modify $ \s -> s { variableTypes = M.union methodTypes (variableTypes s) }
            patType <- nameToType =<< inferPattern pat
            modify $ \s -> s { variableTypes = origTypes }
            -- Infer the RHS type, unify
            (rhsExp, rhsVar) <- inferRhs rhs
            rhsType <- nameToType rhsVar
            unify patType rhsType
            return $ HsPatBind loc pat rhsExp wheres
        HsFunBind matches -> do
            funName <- case matches of
                (HsMatch _ name _ _ _):_ -> return $ convertName name
                _                        -> throwError "No matches in HsFunBind"
            funType <- case M.lookup funName methodTypes of
                Just ft -> nameToType ft
                Nothing -> throwError $ "No type for method " <> showt funName
            (matches', matchTypeVars) <- unzip <$> mapM inferMatch matches
            matchTypes <- mapM nameToType matchTypeVars
            -- Infer the RHS type, match against
            mapM_ (unifyLeft funType) matchTypes
            writeLog $ "Match types: " <> showt matchTypes
            return $ HsFunBind matches'
        _ -> throwError $ unlines ["Illegal declaration in typeclass instance:", synPrint d]

inferMatch :: Syntax.HsMatch -> TypeInferrer (Syntax.HsMatch, TypeVariableName)
inferMatch (HsMatch loc name args rhs wheres) = do
    writeLog $ "Inferring match " <> convertName name <> " at " <> showt loc
    retVar <- freshTypeVarName
    retType <- nameToType retVar
    argVars <- inferPatterns args
    argTypes <- mapM nameToType argVars
    (rhs', rhsVar) <- inferRhs rhs
    rhsType <- nameToType rhsVar
    targetType <- makeFun argTypes rhsType
    unify targetType retType
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
needsRecursiveBinding HsInstDecl{}  = return True
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
    freeTvs <- getFreeTypeVariableNames ds
    mapM_ (\name -> insertQuantifiedType name =<< getQuantifiedType (typeVarMapping M.! name) freeTvs) (S.toList names)
    return declExps

inferDeclGroup :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
inferDeclGroup ds = do
    dependencyGroups <- dependencyOrder ds
    declExps <- forM dependencyGroups $ \group -> do
        boundNames <- getBoundVariables group
        writeLog $ "Processing binding group " <> showtSet boundNames
        inferDecls group
    return $ concat declExps

inferModule :: M.Map TypeVariableName Kind -> M.Map HsQName ClassInfo -> Syntax.HsModule -> TypeInferrer (Syntax.HsModule, M.Map VariableName QuantifiedType)
inferModule ks ci (HsModule p1 p2 p3 p4 decls) = do
    writeLog "-----------------"
    writeLog "- Type Inferrer -"
    writeLog "-----------------"
    modify $ \s -> s { classInfo = ci, kinds = ks }
    writeLog $ "Got class information: " <> showt ci
    let isTypeclassInstance HsInstDecl{} = True
        isTypeclassInstance _            = False
        (instanceDecls, decls') = partition isTypeclassInstance decls
    typePredMap <- M.fromList <$> forM instanceDecls (\d -> (,d) <$> getTypeclassTypePredicate d)
    modify $ \s -> s { typeclassInstances = typePredMap }
    -- Process all the non-instance top-level definitions, then process any instance declarations not already forced to
    -- be processed
    decls'' <- (<>) <$> inferDeclGroup decls' <*> mapM inferDecl instanceDecls
    mapM_ addTypeclassInstanceFromDecl . M.elems =<< gets typeclassInstances
    --writeLog "after all decls"
    --writeLog "---------------------------------------"
    --writeLog "---------------------------------------"
    --writeLog "---------------------------------------"
    --writeLog . showt =<< gets classEnvironment
    --writeLog "---------------------------------------"
    --writeLog "---------------------------------------"
    --writeLog "---------------------------------------"
    let m' = HsModule p1 p2 p3 p4 decls''
    writeLog "Inferred module types"
    ts <- gets bindings
    m'' <- updateModuleTypeTags m'
    writeLog "Updated explicit type tags"
    checkModuleExpTypes m''
    writeLog "Checked explicit type tags"
    return (m'', ts)

getTypeclassTypePredicate :: HsDecl -> TypeInferrer (HsQName, [HsType])
getTypeclassTypePredicate (HsInstDecl _ _ name args _) = return (name, args)
getTypeclassTypePredicate _ = throwError "Unexpected non-typeclass-instance declaration in getTypeclassTypePredicate."

addTypeclassInstanceFromDecl :: HsDecl -> TypeInferrer ()
addTypeclassInstanceFromDecl (HsInstDecl _ ctx name args _) = case (ctx, args) of
    ([], [arg]) -> do
        ks <- getKinds
        t <- synToType ks arg
        addTypeclassInstance $ Qualified S.empty $ IsInstance (convertName name) t
    ([], _) -> throwError "Multiparameter typeclass instances not supported"
    (_, _) -> throwError "Contexts not yet supported in instances"
addTypeclassInstanceFromDecl _ = throwError "Unexpected non-typeclass-instance declaration in addTypeclassInstanceFromDecl."

addTypeclassInstance :: ClassInstance -> TypeInferrer ()
addTypeclassInstance inst = do
    writeLog $ "Adding class instance " <> showt inst
    -- Make sure we use the most resolved type we for the head of the instance
    inst' <- applyCurrentSubstitution inst
    s <- get
    ce' <- TypeClasses.addInstance (classEnvironment s) inst'
    put $ s { classEnvironment = ce' }

inferModuleWithBuiltins :: Syntax.HsModule -> TypeInferrer (Syntax.HsModule, M.Map VariableName QuantifiedType)
inferModuleWithBuiltins m = do
    setClasses builtinClasses
    forM_ (M.toList builtinConstructors ++ M.toList builtinFunctions) (uncurry insertQuantifiedType)
    ci <- getClassInfo m
    inferModule (M.union builtinKinds $ getModuleKinds m) ci m

getAllVariableTypes :: TypeInferrer (M.Map VariableName QuantifiedType)
getAllVariableTypes = do
    binds <- gets bindings
    -- Get the qualified types of each unbound but present variable (and give it an empty quantifier set)
    unboundVariables <- M.toList <$> gets variableTypes
    unbound <- forM unboundVariables $ \(v, t) -> (v,) . Quantified S.empty <$> getQualifiedType t
    return $ M.union binds (M.fromList unbound)


-- |After the 1st pass (`inferModule`) which produced an AST with explicit type tags for each expression, we need to
-- perform this 2nd pass to update the type tags from a stand-in variable to the actual type of the expression.
updateModuleTypeTags :: Syntax.HsModule -> TypeInferrer Syntax.HsModule
updateModuleTypeTags (HsModule a b c d decls) = HsModule a b c d <$> updateDeclsTypeTags decls

updateDeclTypeTags :: Syntax.HsDecl -> TypeInferrer Syntax.HsDecl
updateDeclTypeTags (HsPatBind l pat rhs ds) = HsPatBind l pat <$> updateRhsTypeTags rhs <*> updateDeclsTypeTags ds
updateDeclTypeTags (HsFunBind matches)      = HsFunBind <$> mapM updateMatchTypeTags matches
updateDeclTypeTags d@HsTypeSig{}            = return d
updateDeclTypeTags d@HsClassDecl{}          = return d
updateDeclTypeTags (HsInstDecl loc ctx name ts ds) = HsInstDecl loc ctx name ts <$> updateDeclsTypeTags ds
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
    -- Unless the type tag is an unqualified single type variable as inserted by the 1st pass, ignore it
    qt                         -> return qt
updateExpTypeTags v@HsVar{} = return v
updateExpTypeTags c@HsCon{} = return c
updateExpTypeTags l@HsLit{} = return l
updateExpTypeTags (HsInfixApp e1 op e2) = HsInfixApp <$> updateExpTypeTags e1 <*> pure op <*> updateExpTypeTags e2
updateExpTypeTags (HsApp e1 e2) = HsApp <$> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsNegApp e) = HsNegApp <$> updateExpTypeTags e
updateExpTypeTags (HsLambda l ps e) = HsLambda l ps <$> updateExpTypeTags e
updateExpTypeTags (HsLet ds e) = HsLet <$> updateDeclsTypeTags ds <*> updateExpTypeTags e
updateExpTypeTags (HsIf c e1 e2) = HsIf <$> updateExpTypeTags c <*> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsCase e as) = HsCase <$> updateExpTypeTags e <*> mapM updateAltTypeTags as
updateExpTypeTags (HsTuple es) = HsTuple <$> mapM updateExpTypeTags es
updateExpTypeTags (HsList es) = HsList <$> mapM updateExpTypeTags es
updateExpTypeTags (HsParen e) = HsParen <$> updateExpTypeTags e
updateExpTypeTags e = throwError $ "Unknown expression in updateExpTypeTags: " <> showt e

updateAltTypeTags :: Syntax.HsAlt -> TypeInferrer Syntax.HsAlt
updateAltTypeTags (HsAlt loc pat a wheres) = HsAlt loc pat <$> updateGuardedAltsTypeTags a <*> updateDeclsTypeTags wheres
updateGuardedAltsTypeTags :: Syntax.HsGuardedAlts -> TypeInferrer Syntax.HsGuardedAlts
updateGuardedAltsTypeTags (HsUnGuardedAlt e) = HsUnGuardedAlt <$> updateExpTypeTags e
updateGuardedAltsTypeTags (HsGuardedAlts as) = HsGuardedAlts <$> mapM updateGuardedAltTypeTags as
updateGuardedAltTypeTags :: Syntax.HsGuardedAlt -> TypeInferrer Syntax.HsGuardedAlt
updateGuardedAltTypeTags (HsGuardedAlt loc e1 e2) = HsGuardedAlt loc <$> updateExpTypeTags e1 <*> updateExpTypeTags e2

checkModuleExpTypes :: Syntax.HsModule -> TypeInferrer ()
checkModuleExpTypes (HsModule _ _ _ _ ds) = checkDeclsExpTypes ds
checkDeclsExpTypes :: [Syntax.HsDecl] -> TypeInferrer ()
checkDeclsExpTypes = mapM_ checkDeclExpTypes
checkDeclExpTypes :: Syntax.HsDecl -> TypeInferrer ()
checkDeclExpTypes (HsPatBind _ _ rhs ds) = checkRhsExpTypes rhs >> checkDeclsExpTypes ds
checkDeclExpTypes (HsFunBind matches) = mapM_ checkMatchExpTypes matches
checkDeclExpTypes (HsTypeSig _ names synType) = return ()
--checkDeclExpTypes (HsTypeSig _ names synType) = do
--    ks <- getKinds
--    expectedType <- synToQualType ks synType
--    forM_ (map convertName names) $ \n -> do
--        -- Get fresh type variables in the expected type
--        writeLog "foo"
--        freshExpectedType <- qualToQuant True S.empty expectedType
--        writeLog "bar"
--        inferredType <- instantiate =<< getVariableQuantifiedType n S.empty
--        writeLog "baz"
--        case maybeInferredType of
--            Left _ -> insertQuantifiedType n freshExpectedType -- If there's only a type sig, no definition
--            Right inferredType -> do
--                -- There's a type sig and a definition: if we can't match the inferred type to the sig, error. Otherwise
--                -- update the type with the "at most as restrictive" annotated type
--                if hasMatch inferredType expectedType then
--                    insertQuantifiedType n freshExpectedType
--                else throwError $ unlines
--                    [ "Inferred type for " <> convertName n <> " doesn't match explicit type signature: "
--                    , "Inferred: " <> showt inferredType
--                    , "Explicit: " <> showt expectedType ]

checkDeclExpTypes HsClassDecl{}          = return ()
checkDeclExpTypes (HsInstDecl _ _ _ _ ds) = checkDeclsExpTypes ds
checkDeclExpTypes HsDataDecl{}           = return ()
checkDeclExpTypes _ = throwError "Unsupported declaration when checking user-defined explicit type signatures."
checkRhsExpTypes :: Syntax.HsRhs -> TypeInferrer ()
checkRhsExpTypes (HsUnGuardedRhs e) = checkExpExpTypes e
checkRhsExpTypes (HsGuardedRhss _) = throwError "Unsupported RHS when checking user-defined explicit type signatures."
checkMatchExpTypes :: Syntax.HsMatch -> TypeInferrer ()
checkMatchExpTypes (HsMatch _ _ _ rhs wheres) = checkRhsExpTypes rhs >> checkDeclsExpTypes wheres

checkExpExpTypes :: Syntax.HsExp -> TypeInferrer ()
-- We previously wrapped the manual type signature with an automatically added one, treat them differently
checkExpExpTypes (HsExpTypeSig _ (HsExpTypeSig _ e manualType) autoType)
    | alphaEq manualType autoType = checkExpExpTypes e
    | otherwise = throwError $ "Type mismatch in explicit type annotation: tagged with " <> x <> " but inferred " <> y
        where x = synPrint manualType
              y = synPrint autoType
checkExpExpTypes (HsExpTypeSig _ e _) = checkExpExpTypes e -- Ignore automatically added signatures
checkExpExpTypes HsVar{} = return ()
checkExpExpTypes HsCon{} = return ()
checkExpExpTypes HsLit{} = return ()
checkExpExpTypes (HsInfixApp e1 _ e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsApp e1 e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsNegApp e) = checkExpExpTypes e
checkExpExpTypes (HsLambda _ _ e) = checkExpExpTypes e
checkExpExpTypes (HsLet ds e) = checkDeclsExpTypes ds >> checkExpExpTypes e
checkExpExpTypes (HsIf c e1 e2) = checkExpExpTypes c >> checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsCase e as) = checkExpExpTypes e >> mapM_ checkAltExpTypes as
checkExpExpTypes (HsTuple es) = mapM_ checkExpExpTypes es
checkExpExpTypes (HsList es) = mapM_ checkExpExpTypes es
checkExpExpTypes (HsParen e) = checkExpExpTypes e
checkExpExpTypes e = throwError $ "Unknown expression in checkExpExpTypes: " <> showt e

checkAltExpTypes :: Syntax.HsAlt -> TypeInferrer ()
checkAltExpTypes (HsAlt _ _ a wheres) = checkGuardedAltsExpTypes a >> checkDeclsExpTypes wheres
checkGuardedAltsExpTypes :: Syntax.HsGuardedAlts -> TypeInferrer ()
checkGuardedAltsExpTypes (HsUnGuardedAlt e) = checkExpExpTypes e
checkGuardedAltsExpTypes (HsGuardedAlts as) = mapM_ checkGuardedAltExpTypes as
checkGuardedAltExpTypes :: Syntax.HsGuardedAlt-> TypeInferrer ()
checkGuardedAltExpTypes (HsGuardedAlt _ e1 e2) = mapM_ checkExpExpTypes [e1, e2]
