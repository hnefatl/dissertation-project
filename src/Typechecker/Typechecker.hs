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

-- |Maps globally unique names of functions/variables/data constructors to their instantiated (for variables) or
-- uninstantiated (for functions+constructors) types.
type TypeMap = M.Map Id (Either InstantiatedType UninstantiatedQualifiedType)

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState
    { subs :: Substitution
    , types :: TypeMap
    , classEnvironment :: ClassEnvironment
    , typePredicates :: S.Set InstantiatedTypePredicate
    , variableCounter :: Int }
    deriving (Show)

instance Default InferrerState where
    def = InferrerState
            { subs = def
            , types = M.empty
            , classEnvironment = M.empty
            , typePredicates = S.empty
            , variableCounter = 1 }

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

-- |Creates a fresh (uniquely named) type
freshVariable :: Kind -> TypeInferrer InstantiatedType
freshVariable kind = freshName >>= \name -> return $ TypeVar (TypeVariable name kind)

-- |Returns the current substitution in the monad
getSubstitution :: TypeInferrer Substitution
getSubstitution = gets subs

getClassEnvironment :: TypeInferrer ClassEnvironment
getClassEnvironment = gets classEnvironment

getPredicates :: TypeInferrer (S.Set InstantiatedTypePredicate)
getPredicates = gets typePredicates

addClasses :: ClassEnvironment -> TypeInferrer ()
addClasses env = modify (\s -> s { classEnvironment = M.union env (classEnvironment s) })

addTypes :: TypeMap -> TypeInferrer ()
addTypes vs = modify (\s -> s { types = M.union vs (types s) })
addType :: Id -> Either InstantiatedType UninstantiatedQualifiedType -> TypeInferrer ()
addType name t = addTypes (M.singleton name t)

addConstructorType :: Id -> UninstantiatedQualifiedType -> TypeInferrer ()
addConstructorType name t = addType name (Right t)
addFunctionType :: Id -> UninstantiatedQualifiedType -> TypeInferrer ()
addFunctionType name t = addType name (Right t)
addVariableType :: Id -> InstantiatedType -> TypeInferrer()
addVariableType name t = addType name (Left t)

addTypePredicates :: S.Set InstantiatedTypePredicate -> TypeInferrer ()
addTypePredicates ps = modify (\s -> s { typePredicates = S.union ps (typePredicates s) })
addTypePredicate :: InstantiatedTypePredicate -> TypeInferrer ()
addTypePredicate = addTypePredicates . S.singleton

getType :: Id -> TypeInferrer (Either InstantiatedType UninstantiatedQualifiedType)
getType name = do
    t <- gets (M.lookup name . types)
    maybe (throwError $ printf "Symbol %s not in environment" name) return t

-- These should instantiate the type and add the qualifiers automatically
instantiateConstructor, instantiateFunction :: Id -> TypeInferrer InstantiatedType
(instantiateConstructor, instantiateFunction) = (helper "constructor", helper "function")
    where helper s name = do
            t <- getType name >>= either (const $ throwError $ printf "Got instantiated type for %s: %s" s name) return
            Qualified quals t' <- doInstantiate t
            addTypePredicates quals
            return t'
getVariableType :: Id -> TypeInferrer InstantiatedType
getVariableType name = do
    et <- getType name
    either return (const $ throwError $ printf "Got uninstantiated type for variable %s" name) et

-- Return either a variable, function, or constructor. Instantiate and store qualifiers if necessary
getInstantiatedType :: Id -> TypeInferrer InstantiatedType
getInstantiatedType name = getType name >>= \case
    Left t -> return t
    Right uninst -> do
        Qualified quals t <- doInstantiate uninst
        addTypePredicates quals
        return t


-- |Extend the current substitution with an mgu that unifies the two arguments
-- |Same as unify but only unifying variables in the first argument with those in the left
unify, doMatch :: InstantiatedType -> InstantiatedType -> TypeInferrer ()
(unify, doMatch) = (helper mgu, helper match)
    where helper f t1 t2 = do
            currentSub <- getSubstitution
            newSub <- f (applySub currentSub t1) (applySub currentSub t2)
            modify (\s -> s { subs = subCompose currentSub newSub })


inferLiteral :: Syntax.HsLiteral -> TypeInferrer InstantiatedType
inferLiteral (HsChar _) = return typeChar
inferLiteral (HsString _) = return typeString
inferLiteral (HsInt _) = do
    v <- freshVariable KindStar
    addTypePredicate (IsInstance "Num" v)
    return v
inferLiteral (HsFrac _) = do
    v <- freshVariable KindStar
    addTypePredicate (IsInstance "Fractional" v)
    return v
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

-- TODO(kc506): Change from using the raw syntax expressions to using an intermediate form with eg. lambdas converted
-- into lets? Pg. 26 of thih.pdf
inferExpression :: Syntax.HsExp -> TypeInferrer InstantiatedType
inferExpression (HsVar name) = getInstantiatedType (toId name)
inferExpression (HsCon name) = instantiateConstructor (toId name)
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsParen e) = inferExpression e
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, then use unification to deduce the return type
    funtype <- inferExpression f
    argtype <- inferExpression e
    retType <- freshVariable KindStar
    unify (makeFun [argtype] retType) funtype
    return retType
inferExpression (HsInfixApp lhs op rhs) = do
    let opName = case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp (HsVar opName) lhs) rhs)
inferExpression (HsTuple exps) = makeTuple <$> mapM inferExpression exps
inferExpression (HsLambda _ pats e) = makeFun <$> inferPatterns pats <*> inferExpression e
inferExpression (HsLet decls e) = do
    -- Process the declarations first (bring into scope any variables etc)
    mapM_ inferDecl decls
    inferExpression e
inferExpression (HsIf c e1 e2) = undefined
inferExpression e = throwError ("Unsupported expression: " ++ show e)


inferPattern :: Syntax.HsPat -> TypeInferrer InstantiatedType
inferPattern (HsPVar name) = do
    t <- freshVariable KindStar
    addVariableType (toId name) t
    return t
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = freshVariable KindStar
inferPattern (HsPAsPat name pat) = do
    t <- inferPattern pat
    addVariableType (toId name) t
    return t
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp constructor pats) = do
    -- Infer any nested patterns
    argTypes <- inferPatterns pats
    -- Get the type of the constructor, instantiate it and add the constraints on the new type variables
    constructorType <- instantiateConstructor (toId constructor)
    -- Check we have the right number of arguments to the data constructor
    let expArgCount = getKindArgCount $ getKind constructorType
        argCount = length argTypes
    when (expArgCount /= argCount) (throwError $ printf "Function expected %d args, got %d" expArgCount argCount)
    -- Unify the expected type with the variables we have to match up their types
    returnType <- freshVariable KindStar
    let constructedFnType = makeFun argTypes returnType
    unify constructorType constructedFnType
    return returnType
inferPattern (HsPTuple pats) = makeTuple <$> inferPatterns pats
inferPattern p = throwError ("Unsupported pattern: " ++ show p)

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [InstantiatedType]
inferPatterns = mapM inferPattern


-- TODO(kc506): Change the return type to just yield all the constituent qualified types.
-- Might make more useful if this is going to be used for function alternatives as well as case statements, etc.
inferAlternative :: [Syntax.HsPat] -> Syntax.HsExp -> TypeInferrer InstantiatedType
inferAlternative pats e = makeFun <$> inferPatterns pats <*> inferExpression e

inferAlternatives :: [([Syntax.HsPat], Syntax.HsExp)] -> TypeInferrer InstantiatedType
inferAlternatives alts = do
    ts <- mapM (uncurry inferAlternative) alts
    commonType <- freshVariable KindStar
    mapM_ (unify commonType) ts
    return commonType

-- |Infers the type of a pattern binding (eg. `foo = 5`) without an explicit type
inferImplicitPatternBinding :: Syntax.HsPat -> Syntax.HsRhs -> TypeInferrer ()
inferImplicitPatternBinding pat (HsUnGuardedRhs e) = do
    pattype <- inferPattern pat
    rhstype <- inferExpression e
    unify pattype rhstype
    -- TODO(kc506): Check (alpha, don't unify) that qualifiers match?

    --classEnv <- getClassEnvironment
    --preds <- applySub sub <$> getPredicates
    --let rhsTypeVariables = S.fromList $ getTypeVars $ applySub sub rhsType
    --(qualifiers, _) <- split classEnv rhsTypeVariables preds
    -- TODO(kc506): Set the set of assumptions in the state to be exactly the deferred assumptions?
    --mapM_ (uncurry addVariableType)
    return ()
inferImplicitPatternBinding _ (HsGuardedRhss _) = throwError "Guarded patterns aren't yet supported"

inferDecl :: Syntax.HsDecl -> TypeInferrer ()
inferDecl (HsPatBind _ pat rhs _) = inferImplicitPatternBinding pat rhs
inferDecl (HsFunBind _) = throwError "Function declarations not supported"
inferDecl _ = throwError "Declaration not supported"

getVariableTypes :: TypeInferrer (M.Map Id QualifiedType)
getVariableTypes = do
    sub <- getSubstitution
    -- TODO(kc506): Need to get just **user-defined** variables and functions.......
    -- Get only the qualified variables
    variables <- applySub sub . M.mapMaybe (either Just (const Nothing)) <$> gets types
    classEnv <- getClassEnvironment
    predicates <- applySub sub <$> getPredicates
    --s <- get
    --traceM (unpack $ pShow s)
    let relevantPred t (IsInstance _ x) = S.fromList (getTypeVars x) `S.isSubsetOf` S.fromList (getTypeVars t)
        getRelevantPreds t = simplify classEnv $ S.filter (relevantPred t) predicates
    mapM (\t -> Qualified <$> getRelevantPreds t <*> pure t) variables