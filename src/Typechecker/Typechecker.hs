{-# Language FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TupleSections #-}

module Typechecker.Typechecker where

import ExtraDefs
import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Typeclasses
import Typechecker.Simplifier
import Preprocessor.Renamer (getDeclsBoundNames)

import Text.Printf
import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Default
import Data.Foldable
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M

import Language.Haskell.Syntax as Syntax

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
newtype TypeInferrer a = TypeInferrer (ExceptT String (State InferrerState) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadState InferrerState, MonadError String)
instance NameGenerator TypeInferrer TypeVariableName where
    freshName = do
        counter <- gets variableCounter
        modify (\s -> s { variableCounter = counter + 1 })
        return (TypeVariableName $ "t" ++ show counter)

-- |Run type inference, and return the (possible failed) result along with the last state
runTypeInferrer :: MonadError String m => TypeInferrer a -> (m a, InferrerState)
runTypeInferrer (TypeInferrer inner) = (liftEither x, s)
    where (x, s) = runState (runExceptT inner) def

-- |Run type inference, and return the (possibly failed) result
evalTypeInferrer :: MonadError String m => TypeInferrer a -> m a
evalTypeInferrer = fst . runTypeInferrer

-- |Run type inference, and return the (possibly failed) state
execTypeInferrer :: MonadError String m => TypeInferrer a -> m InferrerState
execTypeInferrer (TypeInferrer inner) = case e of
    Left err -> throwError err
    Right _ -> return s
    where (e, s) = runState (runExceptT inner) def

writeLog :: String -> TypeInferrer ()
writeLog l = modify (\s -> s { logs = logs s Seq.|> l })
getLogs :: TypeInferrer [String]
getLogs = toList <$> gets logs

-- |Creates a fresh (uniquely named) type variable
freshTypeVariable :: TypeInferrer TypeVariableName
freshTypeVariable = freshName

nameToType :: TypeVariableName -> TypeInferrer Type
nameToType name = TypeVar <$> nameToTypeVariable name
nameToTypeVariable :: TypeVariableName -> TypeInferrer TypeVariable
nameToTypeVariable name = TypeVariable name <$> getTypeVariableKind name

-- |Generate a type variable name and make it refer to the given type.
nameSimpleType :: Type -> TypeInferrer TypeVariableName
nameSimpleType t = do
    name <- freshTypeVariable
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
    tv <- freshTypeVariable
    addVariableType name tv
    return tv

-- |Instantiate a quantified type into a type, replacing all universally quantified variables with new
-- type variables and adding the new type constraints to the environment
instantiate :: QuantifiedType -> TypeInferrer TypeVariableName
instantiate qt@(Quantified _ ql@(Qualified quals t)) = do
    v <- freshTypeVariable
    writeLog $ "Instantiating " ++ show v ++ " with " ++ show qt
    -- Create a "default" mapping from each type variable in the type to itself
    let identityMap = M.fromList $ map (\x -> (x, TypeVar $ TypeVariable x KindStar)) $ S.toList $ getTypeVars ql
    -- Create a substitution for each quantified variable to a fresh name, using the identity sub as default
    sub <- Substitution <$> (M.union <$> getInstantiatingTypeMap qt <*> pure identityMap)
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
addTypePredicate (IsInstance _ TypeConstant{}) = throwError "Not implemented"
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
          helper old TypeConstant{} = do
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

getVariableQuantifiedType :: VariableName -> TypeInferrer QuantifiedType
getVariableQuantifiedType name = getQuantifiedType =<< getVariableTypeVariable name

getQuantifiedType :: TypeVariableName -> TypeInferrer QuantifiedType
getQuantifiedType name = do
    ce <- getClassEnvironment
    t <- applyCurrentSubstitution =<< nameToType name
    let typeVars = getTypeVars t
    predicates <- simplify ce =<< S.unions <$> mapM getTypePredicates (S.toList typeVars)
    quantifiers <- S.fromList <$> mapM nameToTypeVariable (S.toList typeVars)
    let qt = Quantified quantifiers $ Qualified predicates t
    writeLog $ printf "%s has quantified type %s" (show name) (show qt)
    return qt


inferLiteral :: Syntax.HsLiteral -> TypeInferrer TypeVariableName
inferLiteral (HsChar _) = nameSimpleType typeChar
inferLiteral (HsString _) = nameSimpleType typeString
inferLiteral (HsInt _) = do
    v <- freshTypeVariable
    addTypeConstraint v (TypeConstantName "Num")
    return v
inferLiteral (HsFrac _) = do
    v <- freshTypeVariable
    addTypeConstraint v (TypeConstantName "Fractional")
    return v
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

-- TODO(kc506): Change from using the raw syntax expressions to using an intermediate form with eg. lambdas converted
-- into lets? Pg. 26 of thih.pdf
inferExpression :: Syntax.HsExp -> TypeInferrer TypeVariableName
inferExpression (HsVar name) = getVariableTypeVariable (convertName name)
inferExpression (HsCon name) = getVariableTypeVariable (convertName name)
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsParen e) = inferExpression e
inferExpression (HsLambda _ pats e) = do
    retVar <- freshTypeVariable
    retType <- nameToType retVar
    argVars <- inferPatterns pats
    argTypes <- mapM nameToType argVars
    bodyType <- nameToType =<< inferExpression e
    unify (makeFun argTypes bodyType) retType
    return retVar
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, and instantiate any quantified variables
    funVar <- inferExpression f
    argVar <- inferExpression e
    -- Generate a fresh variable for the return type
    retVar <- freshTypeVariable
    [retType, funType, argType] <- mapM nameToType [retVar, funVar, argVar]
    -- Unify `function` with `argument -> returntype` to match up the types.
    unify (makeFun [argType] retType) funType
    return retVar
inferExpression (HsInfixApp lhs op rhs) = do
    let opName = case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp (HsVar opName) lhs) rhs)
inferExpression (HsTuple exps) = do
    expTypes <- mapM nameToType =<< mapM inferExpression exps
    retVar <- freshTypeVariable
    retType <- nameToType retVar
    unify retType (makeTuple expTypes)
    return retVar
inferExpression (HsLet decls e) = do
    -- Process the declarations first (bring into scope any variables etc)
    inferDeclGroup decls
    inferExpression e
inferExpression (HsIf c e1 e2) = do
    ct <- getQuantifiedType =<< inferExpression c
    let expectedType = Quantified S.empty (Qualified S.empty typeBool)
    unless (ct == expectedType) (throwError $ printf "`if` expression condition %s doesn't have type bool" (show c))
    e1t <- nameToType =<< inferExpression e1
    e2t <- nameToType =<< inferExpression e2
    commonVar <- freshTypeVariable
    commonType <- nameToType commonVar
    unify commonType e1t
    unify commonType e2t
    return commonVar
inferExpression (HsList exps) = do
    ets <- mapM nameToType =<< mapM inferExpression exps
    -- Check each element has the same type
    commonType <- nameToType =<< freshTypeVariable
    mapM_ (unify commonType) ets
    v <- freshTypeVariable
    vt <- nameToType v
    unify vt (makeList commonType)
    return v
inferExpression e = throwError ("Unsupported expression: " ++ show e)


inferPattern :: Syntax.HsPat -> TypeInferrer TypeVariableName
inferPattern (HsPVar name) = getVariableTypeVariableOrAdd (convertName name)
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = freshTypeVariable
inferPattern (HsPAsPat name pat) = do
    t <- nameToType =<< inferPattern pat
    v <- getVariableTypeVariableOrAdd (convertName name)
    vt <- nameToType v
    unify t vt
    return v
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp _ _) = throwError "Currently not supported"
inferPattern (HsPTuple pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    v <- freshTypeVariable
    vt <- nameToType v
    unify vt (makeTuple pts)
    return v
inferPattern (HsPList pats) = do
    pts <- mapM nameToType =<< inferPatterns pats
    -- Check each element has the same type
    commonType <- nameToType =<< freshTypeVariable
    mapM_ (unify commonType) pts
    v <- freshTypeVariable
    vt <- nameToType v
    unify vt (makeList commonType)
    return v
inferPattern p = throwError ("Unsupported pattern: " ++ show p)

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [TypeVariableName]
inferPatterns = mapM inferPattern


inferAlternative :: [Syntax.HsPat] -> Syntax.HsExp -> TypeInferrer TypeVariableName
inferAlternative pats e = do
    retVar <- freshTypeVariable
    retType <- nameToType retVar
    patTypes <- mapM nameToType =<< inferPatterns pats
    exprType <- nameToType =<< inferExpression e
    unify (makeFun patTypes retType) exprType
    return retVar

inferAlternatives :: [([Syntax.HsPat], Syntax.HsExp)] -> TypeInferrer TypeVariableName
inferAlternatives alts = do
    ts <- mapM nameToType =<< mapM (uncurry inferAlternative) alts
    commonVar <- freshTypeVariable
    commonType <- nameToType commonVar
    mapM_ (unify commonType) ts
    return commonVar

inferRhs :: Syntax.HsRhs -> TypeInferrer TypeVariableName
inferRhs (HsUnGuardedRhs e) = inferExpression e
inferRhs (HsGuardedRhss _) = throwError "Guarded patterns aren't yet supported"

inferDecl :: Syntax.HsDecl -> TypeInferrer ()
inferDecl (HsPatBind _ pat rhs _) = do
    patType <- nameToType =<< inferPattern pat
    rhsType <- nameToType =<< inferRhs rhs
    unify patType rhsType
inferDecl (HsFunBind _) = throwError "Functions not yet supported"
inferDecl _ = throwError "Declaration not yet supported"

inferDeclGroup :: [Syntax.HsDecl] -> TypeInferrer ()
inferDeclGroup ds = do
    names <- getDeclsBoundNames ds
    typeVarMapping <- M.fromList <$> mapM (\n -> (n,) <$> freshName) (S.toList names)
    writeLog $ printf "Adding %s to environment for declaration group" (show typeVarMapping)
    addVariableTypes typeVarMapping
    mapM_ inferDecl ds
    mapM_ (\name -> insertQuantifiedType name =<< getQuantifiedType (typeVarMapping M.! name)) (S.toList names)

-- TODO(kc506): Dependency analysis, split into typechecking groups and process individually
-- TODO(kc506): Take advantage of explicit type hints
inferModule :: Syntax.HsModule -> TypeInferrer ()
inferModule (HsModule _ _ _ _ decls) = inferDeclGroup decls


getVariableTypes :: TypeInferrer (M.Map VariableName QuantifiedType)
getVariableTypes = do
    writeLog "Getting variable types"
    binds <- gets bindings
    locals <- gets variableTypes
    -- Quantify each variable that's not used in a binding, then strip the quantifiers, as only bound variables get
    -- quantifiers.
    let stripQuantifiers varName = do
            Quantified _ qual <- getVariableQuantifiedType varName
            return (varName, Quantified S.empty qual)
    locals' <- M.fromList <$> mapM stripQuantifiers (M.keys locals)
    return $ M.union binds locals'