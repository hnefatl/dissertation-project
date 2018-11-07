{-# Language FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase #-}

module Typechecker.Typechecker where

import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Typeclasses
import Typechecker.Simplifier
import ExtraDefs

import Data.Default
import Text.Printf
import Control.Monad.Except
import Control.Monad.State.Strict
import Debug.Trace
import Text.Pretty.Simple
import Data.Text.Lazy (unpack)
import qualified Data.Set as S
import qualified Data.Map as M

import Language.Haskell.Syntax as Syntax

-- |Maps globally unique names of functions/variables/data constructors to a type variable representing their type.
type TypeMap = M.Map VariableName TypeVariableName

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState
    { substitutions :: Substitution
    , types :: TypeMap
    , classEnvironment :: ClassEnvironment
    , kinds :: M.Map TypeVariableName Kind
    , typePredicates :: M.Map TypeVariableName (S.Set ClassName)
    , freeVariables :: S.Set TypeVariableName
    , variableCounter :: Int }
    deriving (Show)

instance Default InferrerState where
    def = InferrerState
            { substitutions = def
            , types = M.empty
            , classEnvironment = M.empty
            , kinds = M.empty
            , typePredicates = M.empty
            , freeVariables = S.empty
            , variableCounter = 0 }

-- |A TypeInferrer handles mutable state and error reporting
newtype TypeInferrer a = TypeInferrer (ExceptT String (State InferrerState) a)
    deriving (Functor, Applicative, Monad, MonadState InferrerState, MonadError String)

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

instance TypeInstantiator TypeInferrer where
    freshName = do
        counter <- (1 +) <$> gets variableCounter
        modify (\s -> s { variableCounter = counter })
        return ("v" ++ show counter)

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

-- |If the variable's not in the map, default to it being *: we only store interesting kinds for efficiency.
getTypeVariableKind :: TypeVariableName -> TypeInferrer Kind
getTypeVariableKind name = gets (M.findWithDefault KindStar name . kinds)

getClassEnvironment :: TypeInferrer ClassEnvironment
getClassEnvironment = gets classEnvironment

getFreeVariables :: TypeInferrer (S.Set TypeVariableName)
getFreeVariables = gets freeVariables

addFreeVariable :: TypeVariableName -> TypeInferrer ()
addFreeVariable = addFreeVariables . S.singleton
addFreeVariables :: S.Set TypeVariableName -> TypeInferrer ()
addFreeVariables names = modify (\s -> s { freeVariables = S.union names (freeVariables s) })

getConstraints :: TypeVariableName -> TypeInferrer (S.Set ClassName)
getConstraints name = gets (M.findWithDefault S.empty name . typePredicates)

addClasses :: ClassEnvironment -> TypeInferrer ()
addClasses env = modify (\s -> s { classEnvironment = M.union env (classEnvironment s) })

addVariableType :: VariableName -> TypeVariableName -> TypeInferrer ()
addVariableType name t = addVariableTypes (M.singleton name t)
addVariableTypes :: TypeMap -> TypeInferrer ()
addVariableTypes vs = modify (\s -> s { types = M.union vs (types s) })

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
insertQuantifiedType name t@(Quantified quants qualType) = do
    nameSub <- getInstantiatingMap t
    let typeSub = M.map (\name -> TypeVar (TypeVariable name KindStar)) nameSub
    forM_ quants $ \(TypeVariable old _) -> addFreeVariable (nameSub M.! old)
    let Qualified quals t' = applySub (Substitution typeSub) qualType
    addTypePredicates quals
    funVar <- freshTypeVariable
    funType <- nameToType funVar
    unify funType t'
    addVariableType name funVar

-- |Given a variable name, get the type variable name that corresponds
getVariableTypeVariable :: VariableName -> TypeInferrer TypeVariableName
getVariableTypeVariable name = do
    t <- gets (M.lookup name . types)
    maybe (throwError $ printf "Symbol %s not in environment" name) return t

-- |Given a substitution, propagate constraints on the "from" of the substitution to the "to" of the substitution: eg.
-- if we have `Num t1` and `[t2/t1]` we add a constraint `Num t2`, and if we have `instance (Foo a, Bar b) => Baz (Maybe
-- a b)`, `Foo t1` and `[(Maybe t2 t3)/t1]` then we add constraints `Foo t2` and `Bar t3`.
updateTypeConstraints :: Substitution -> TypeInferrer ()
updateTypeConstraints sub@(Substitution mapping) = forM_ (M.toList mapping) (uncurry helper)
    where helper old (TypeVar (TypeVariable new _)) = addTypeConstraints new =<< getConstraints old
          helper old TypeConstant{} = do
            constraints <- getConstraints old
            forM_ constraints $ \classInstance -> do
                ce <- getClassEnvironment
                -- Reconstruct the type predicate, apply the substitution, find which constraints it implies
                pred <- applySub sub <$> (IsInstance classInstance <$> nameToType old)
                newPredicates <- ifPThenByInstance ce pred >>= \case
                    Nothing -> throwError $ "No matching instance for " ++ show pred
                    Just cs -> return cs
                detectInvalidPredicates ce newPredicates
                addTypePredicates newPredicates

-- |Extend the current substitution with an mgu that unifies the two arguments
-- |Same as unify but only unifying variables in the first argument with those in the left
unify :: Type -> Type -> TypeInferrer ()
unify t1 t2 = do
    currentSub <- getSubstitution
    newSub <- mgu (applySub currentSub t1) (applySub currentSub t2)
    updateTypeConstraints newSub
    composeSubstitution newSub


getTypePredicates :: TypeVariableName -> TypeInferrer (S.Set TypePredicate)
getTypePredicates name = do
    constraints <- getConstraints name
    S.fromList <$> mapM (\classname -> IsInstance classname <$> nameToType name) (S.toList constraints)

getVariableType :: VariableName -> TypeInferrer QuantifiedType
getVariableType name = getType =<< getVariableTypeVariable name

getType :: TypeVariableName -> TypeInferrer QuantifiedType
getType name = do
    sub <- getSubstitution
    ce <- getClassEnvironment
    t <- applySub sub <$> nameToType name
    let typeVars = getTypeVars t
    predicates <- simplify ce =<< S.unions <$> mapM getTypePredicates (S.toList typeVars)
    let qualType = Qualified predicates t
    quantifierVars <- S.intersection typeVars <$> getFreeVariables
    quantifiers <- S.fromList <$> mapM nameToTypeVariable (S.toList quantifierVars)
    return $ Quantified quantifiers (Qualified predicates t)


inferLiteral :: Syntax.HsLiteral -> TypeInferrer TypeVariableName
inferLiteral (HsChar _) = nameSimpleType typeChar
inferLiteral (HsString _) = nameSimpleType typeString
inferLiteral (HsInt _) = do
    v <- freshTypeVariable
    addFreeVariable v
    addTypeConstraint v "Num"
    return v
inferLiteral (HsFrac _) = do
    v <- freshTypeVariable
    addFreeVariable v
    addTypeConstraint v "Fractional"
    return v
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

-- TODO(kc506): Change from using the raw syntax expressions to using an intermediate form with eg. lambdas converted
-- into lets? Pg. 26 of thih.pdf
inferExpression :: Syntax.HsExp -> TypeInferrer TypeVariableName
inferExpression (HsVar name) = getVariableTypeVariable (toId name)
inferExpression (HsCon name) = getVariableTypeVariable (toId name)
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsParen e) = inferExpression e
inferExpression (HsLambda _ pats e) = do
    retVar <- freshTypeVariable
    retType <- nameToType retVar
    argVars <- inferPatterns pats
    argTypes <- mapM nameToType argVars
    bodyType <- nameToType =<< inferExpression e
    unify (makeFun argTypes bodyType) retType
    addFreeVariables (S.fromList argVars)
    return retVar
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, and instantiate any quantified variables
    funQuant@(Quantified _ funQual) <- getType =<< inferExpression f
    funSub <- Substitution <$> getInstantiatingTypeMap funQuant
    let Qualified _ funType = applySub funSub funQual
    argQuant@(Quantified _ argQual) <- getType =<< inferExpression e
    argSub <- Substitution <$> getInstantiatingTypeMap argQuant
    let Qualified _ argType = applySub argSub argQual
    -- Generate a fresh variable for the return type
    retVar <- freshTypeVariable
    let retType = TypeVar (TypeVariable retVar KindStar)
    -- Unify `function` with `argument -> returntype` to match up the types.
    sub <- mgu (makeFun [argType] retType) funType
    -- Update our running substitution with this new one.
    composeSubstitution sub
    -- Add new type constraints
    -- We propagate constraints from the function/argument instantiating substitutions, but we make sure not to add
    -- these to the global substitution: otherwise free variables become bound after a single use.
    updateTypeConstraints funSub
    updateTypeConstraints argSub
    updateTypeConstraints sub
    --traceM $ printf "%s %s %s %s" retVar (show funSub) (show argSub) (show sub)
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
    mapM_ inferDecl decls
    inferExpression e
inferExpression (HsIf c e1 e2) = do
    ct <- getType =<< inferExpression c
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
inferPattern (HsPVar name) = do
    v <- freshTypeVariable
    addVariableType (toId name) v
    return v
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = freshTypeVariable
inferPattern (HsPAsPat name pat) = do
    t <- inferPattern pat
    addVariableType (toId name) t
    return t
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp constructor pats) = throwError "Currently not supported"
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

-- |Infers the type of a pattern binding (eg. `foo = 5`) without an explicit type
inferImplicitPatternBinding :: Syntax.HsPat -> Syntax.HsRhs -> TypeInferrer ()
inferImplicitPatternBinding pat (HsUnGuardedRhs e) = do
    patType <- nameToType =<< inferPattern pat
    rhsType <- nameToType =<< inferExpression e
    unify patType rhsType
inferImplicitPatternBinding _ (HsGuardedRhss _) = throwError "Guarded patterns aren't yet supported"

inferDecl :: Syntax.HsDecl -> TypeInferrer ()
inferDecl (HsPatBind _ pat rhs _) = inferImplicitPatternBinding pat rhs
inferDecl (HsFunBind _) = throwError "Function declarations not supported"
inferDecl _ = throwError "Declaration not supported"

getVariableTypes :: TypeInferrer (M.Map TypeVariableName QuantifiedType)
getVariableTypes = mapM getType =<< gets types