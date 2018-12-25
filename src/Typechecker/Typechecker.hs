{-# Language FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TupleSections #-}

module Typechecker.Typechecker where

import Text.Printf
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default
import Data.Foldable
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M
import Language.Haskell.Syntax as Syntax
import Language.Haskell.Pretty

import AlphaEq
import ExtraDefs
import Names
import NameGenerator
import Typechecker.Hardcoded
import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Typeclasses
import Typechecker.Simplifier
import Preprocessor.ContainedNames (getDeclsBoundNames)
import Preprocessor.Dependency


-- |Maps globally unique names of functions/variables/data constructors to a type variable representing their type.
type TypeMap = M.Map VariableName TypeVariableName

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState
    { substitutions :: Substitution
    , variableTypes :: M.Map VariableName TypeVariableName -- "In progress" variable -> type variable mappings
    , bindings :: M.Map VariableName QuantifiedType -- "Finished" variable -> fully described quantified type mappings
    , classEnvironment :: ClassEnvironment
    , kinds :: M.Map TypeVariableName Kind
    , typePredicates :: M.Map TypeVariableName (S.Set ClassName)
    , variableCounter :: Int
    , logs :: Seq.Seq String }
    deriving (Show)

instance Default InferrerState where
    def = InferrerState
            { substitutions = def
            , variableTypes = M.empty
            , bindings = M.empty
            , classEnvironment = M.empty
            , kinds = M.empty
            , typePredicates = M.empty
            , variableCounter = 0
            , logs = Seq.empty }

-- |A TypeInferrer handles mutable state and error reporting
newtype TypeInferrer a = TypeInferrer (ExceptT String (StateT InferrerState NameGenerator) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadState InferrerState, MonadError String, MonadNameGenerator)

-- |Run type inference, and return the (possible failed) result along with the last state
runTypeInferrer :: TypeInferrer a -> NameGenerator (Except String a, InferrerState)
runTypeInferrer (TypeInferrer inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalTypeInferrer :: TypeInferrer a -> ExceptT String NameGenerator a
evalTypeInferrer (TypeInferrer inner) = do
    x <- lift $ evalStateT (runExceptT inner) def
    liftEither x


writeLog :: String -> TypeInferrer ()
writeLog l = modify (\s -> s { logs = logs s Seq.|> l })
getLogs :: TypeInferrer [String]
getLogs = toList <$> gets logs

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
    unless (M.null inter) (throwError $ printf "Overwriting variables %s" (show inter))
    modify (\s -> s { variableTypes = M.union vs vts })
-- |Given a variable name, get the type variable name that corresponds
getVariableTypeVariable :: VariableName -> TypeInferrer TypeVariableName
getVariableTypeVariable name = do
    x <- gets (M.lookup name . variableTypes) -- Variable's either provided by the variableTypes
    y <- traverse instantiate =<< gets (M.lookup name . bindings) -- Or the bindings
    maybe (throwError $ printf "Symbol %s not in environment" (show name)) return (x <|> y)
getVariableTypeVariableOrAdd :: VariableName -> TypeInferrer TypeVariableName
getVariableTypeVariableOrAdd name = catchError (getVariableTypeVariable name) $ \_ -> do
    tv <- freshTypeVarName
    addVariableType name tv
    return tv

-- |Instantiate a quantified type into a simple type, replacing all universally quantified variables with new
-- type variables and adding the new type constraints to the environment
instantiate :: QuantifiedType -> TypeInferrer TypeVariableName
instantiate qt@(Quantified _ ql@(Qualified quals t)) = do
    v <- freshTypeVarName
    -- Create a "default" mapping from each type variable in the type to itself
    let identityMap = M.fromList $ map (\x -> (x, TypeVar $ TypeVariable x KindStar)) $ S.toList $ getTypeVars ql
    -- Create a substitution for each quantified variable to a fresh name, using the identity sub as default
    sub <- Substitution <$> (M.union <$> getInstantiatingTypeMap qt <*> pure identityMap)
    writeLog $ printf "Instantiating %s with %s using %s" (show v) (show qt) (show sub)
    addTypePredicates $ S.map (applySub sub) quals
    vt <- nameToType v
    unify vt (applySub sub t)
    return v

addTypeConstraint :: TypeVariableName -> ClassName -> TypeInferrer ()
addTypeConstraint varname classname = addTypeConstraints varname (S.singleton classname)
addTypeConstraints:: TypeVariableName -> S.Set ClassName -> TypeInferrer ()
addTypeConstraints name preds = mergeTypeConstraints (M.singleton name preds)
mergeTypeConstraints :: M.Map TypeVariableName (S.Set ClassName) -> TypeInferrer ()
mergeTypeConstraints ps = modify (\s -> s { typePredicates = M.unionWith S.union ps (typePredicates s) })
addTypePredicate :: TypePredicate -> TypeInferrer ()
addTypePredicate (IsInstance classname (TypeVar (TypeVariable name _))) = addTypeConstraint name classname
addTypePredicate (IsInstance classname (TypeCon (TypeConstant name _))) = addTypeConstraint name classname
addTypePredicate (IsInstance _ TypeApp{}) = throwError "Not implemented, but should be"
addTypePredicates :: S.Set TypePredicate -> TypeInferrer ()
addTypePredicates = mapM_ addTypePredicate

-- |Intended for use in injecting a known named quantified type into the environment
insertQuantifiedType :: VariableName -> QuantifiedType -> TypeInferrer ()
insertQuantifiedType name t = do
    bs <- gets bindings
    when (M.member name bs) (throwError $ printf "Overwriting binding %s with %s" (show name) (show t))
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
                    Nothing -> throwError $ "No matching instance for " ++ show predicate
                    Just cs -> return cs
                detectInvalidPredicates ce newPreds
                return newPreds
            writeLog $ printf "New constraints %s from substitution %s" (show newPredicates) (show sub)
            addTypePredicates newPredicates

-- |Extend the current substitution with an mgu that unifies the two arguments
unify :: Type -> Type -> TypeInferrer ()
unify t1 t2 = do
    writeLog $ "Unifying " ++ show t1 ++ " with " ++ show t2
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
    writeLog $ printf "%s has qualified type %s" (show name) (show qt)
    return qt
getQuantifiedType :: TypeVariableName -> TypeInferrer QuantifiedType
getQuantifiedType name = do
    qualT <- getQualifiedType name
    quantifiers <- S.fromList <$> mapM nameToTypeVariable (S.toList $ getTypeVars qualT)
    qt <- Quantified quantifiers <$> getQualifiedType name
    writeLog $ printf "%s has quantified type %s" (show name) (show qt)
    return qt
getVariableQuantifiedType :: VariableName -> TypeInferrer QuantifiedType
getVariableQuantifiedType name = getQuantifiedType =<< getVariableTypeVariable name

-- |If the given type variable name refers to a quantified type, instantiate it and return the new type variable name.
-- If the name refers to a non-quantified type, just return the same name
instantiateIfNeeded :: TypeVariableName -> TypeInferrer TypeVariableName
instantiateIfNeeded name = gets (reverseLookup name . variableTypes) >>= \case
        Just varName -> gets (M.lookup varName . bindings) >>= \case
            Just qt -> instantiate qt
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
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

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
    inferExpression (HsApp (HsApp (HsVar $ UnQual $ HsIdent name) lhs) rhs)
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
inferExpression (HsIf cond e1 e2) = do
    (condExp, condVar) <- inferExpression cond
    condType <- getQuantifiedType condVar
    let expected = Quantified S.empty (Qualified S.empty typeBool)
    unless (condType == expected) $ throwError $ printf "if condition %s doesn't have type bool" $ show cond
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
inferExpression e = throwError $ "Unsupported expression: " ++ show e


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
    t <- instantiate =<< getVariableQuantifiedType (convertName con)
    conType <- applyCurrentSubstitution =<< nameToType t
    -- Check the data constructor's been fully applied
    let (args, _) = unmakeFun conType
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
inferPattern p = throwError ("Unsupported pattern: " ++ show p)

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [TypeVariableName]
inferPatterns = mapM inferPattern


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
    patType <- nameToType =<< inferPattern pat
    (rhsExp, rhsVar) <- inferRhs rhs
    rhsType <- nameToType rhsVar
    unify patType rhsType
    return $ HsPatBind l pat rhsExp ds
inferDecl (HsFunBind _) = throwError "Functions not yet supported"
inferDecl _ = throwError "Declaration not yet supported"

inferDecls :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
inferDecls ds = do
    names <- getDeclsBoundNames ds
    typeVarMapping <- M.fromList <$> mapM (\n -> (n,) <$> freshTypeVarName) (S.toList names)
    writeLog $ printf "Adding %s to environment for declaration group" (show typeVarMapping)
    addVariableTypes typeVarMapping
    declExps <- mapM inferDecl ds
    -- Update our name -> type mappings
    mapM_ (\name -> insertQuantifiedType name =<< getQuantifiedType (typeVarMapping M.! name)) (S.toList names)
    return declExps

inferDeclGroup :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
inferDeclGroup ds = do
    dependencyGroups <- dependencyOrder ds
    declExps <- forM dependencyGroups $ \group -> do
        boundNames <- S.toList <$> getDeclsBoundNames group
        writeLog $ printf "Processing binding group {%s}" (intercalate "," $ map show boundNames)
        inferDecls group
    return $ concat declExps

-- TODO(kc506): Take advantage of explicit type hints
inferModule :: Syntax.HsModule -> TypeInferrer (Syntax.HsModule, M.Map VariableName QuantifiedType)
inferModule (HsModule a b c d decls) = do
    m <- HsModule a b c d <$> inferDeclGroup decls
    ts <- getBoundVariableTypes
    m' <- updateModuleTypeTags m
    checkModuleExpTypes m'
    return (m', ts)

-- TODO(kc506): Delete once we don't need builtins
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
updateDeclTypeTags _ = throwError "Unsupported declaration when updating type tags"
updateDeclsTypeTags :: [Syntax.HsDecl] -> TypeInferrer [Syntax.HsDecl]
updateDeclsTypeTags = mapM updateDeclTypeTags

updateRhsTypeTags :: Syntax.HsRhs -> TypeInferrer Syntax.HsRhs
updateRhsTypeTags (HsUnGuardedRhs e) = HsUnGuardedRhs <$> updateExpTypeTags e
updateRhsTypeTags (HsGuardedRhss _) = throwError "Unsupported RHS when updating type tags"

updateExpTypeTags :: Syntax.HsExp -> TypeInferrer Syntax.HsExp
updateExpTypeTags (HsExpTypeSig l e t) = HsExpTypeSig l <$> updateExpTypeTags e <*> case t of
    HsQualType [] (HsTyVar tv) -> qualTypeToSyn =<< getQualifiedType (convertName tv)
    qt -> return qt -- Unless the type tag is an unqualified single type variable as inserted by the 1st pass, ignore it
updateExpTypeTags (HsInfixApp e1 op e2) = HsInfixApp <$> updateExpTypeTags e1 <*> pure op <*> updateExpTypeTags e2
updateExpTypeTags (HsApp e1 e2) = HsApp <$> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsNegApp e) = HsNegApp <$> updateExpTypeTags e
updateExpTypeTags (HsLambda l ps e) = HsLambda l ps <$> updateExpTypeTags e
updateExpTypeTags (HsIf c e1 e2) = HsIf <$> updateExpTypeTags c <*> updateExpTypeTags e1 <*> updateExpTypeTags e2
updateExpTypeTags (HsTuple es) = HsTuple <$> mapM updateExpTypeTags es
updateExpTypeTags (HsList es) = HsList <$> mapM updateExpTypeTags es
updateExpTypeTags e = return e

checkModuleExpTypes :: MonadError String m => Syntax.HsModule -> m ()
checkModuleExpTypes (HsModule _ _ _ _ ds) = checkDeclsExpTypes ds
checkDeclExpTypes :: MonadError String m => Syntax.HsDecl -> m ()
checkDeclExpTypes (HsPatBind _ _ rhs ds) = checkRhsExpTypes rhs >> checkDeclsExpTypes ds
checkDeclExpTypes _ = throwError "Unsupported declaration when checking user-defined explicit type signatures."
checkDeclsExpTypes :: MonadError String m => [Syntax.HsDecl] -> m ()
checkDeclsExpTypes = mapM_ checkDeclExpTypes
checkRhsExpTypes :: MonadError String m => Syntax.HsRhs -> m ()
checkRhsExpTypes (HsUnGuardedRhs e) = checkExpExpTypes e
checkRhsExpTypes (HsGuardedRhss _) = throwError "Unsupported RHS when checking user-defined explicit type signatures."
checkExpExpTypes :: MonadError String m => Syntax.HsExp -> m ()
checkExpExpTypes (HsExpTypeSig _ (HsExpTypeSig _ e manualType) autoType)
    | alphaEq manualType autoType = checkExpExpTypes e
    | otherwise = throwError $ printf "Type mismatch in explicit type annotation: tagged with %s but inferred %s." x y
        where x = prettyPrint manualType
              y = prettyPrint autoType
checkExpExpTypes (HsInfixApp e1 _ e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsApp e1 e2) = checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsNegApp e) = checkExpExpTypes e
checkExpExpTypes (HsLambda _ _ e) = checkExpExpTypes e
checkExpExpTypes (HsIf c e1 e2) = checkExpExpTypes c >> checkExpExpTypes e1 >> checkExpExpTypes e2
checkExpExpTypes (HsTuple es) = mapM_ checkExpExpTypes es
checkExpExpTypes (HsList es) = mapM_ checkExpExpTypes es
checkExpExpTypes _ = return ()