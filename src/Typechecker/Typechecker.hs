{-# Language FlexibleContexts, GeneralizedNewtypeDeriving #-}

module Typechecker.Typechecker where

import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Typeclasses
import Typechecker.Simplifier
import ExtraDefs

import Debug.Trace

import Data.Default
import Text.Printf
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Set as S
import qualified Data.Map as M

import Language.Haskell.Syntax as Syntax

-- |Maps globally unique names of functions/variables/data constructors to their instantiated (for variables) or
-- uninstantiated (for functions+constructors) types.
type TypeMap = M.Map Id (Either QualifiedType UninstantiatedQualifiedType)

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

-- |A TypeInferrer handles mutable state, some scoped read-only assumptions about the types of variables, and error
-- reporting
newtype TypeInferrer a = TypeInferrer (StateT InferrerState (Except String) a)
    deriving (Functor, Applicative, Monad, MonadState InferrerState, MonadError String)

runTypeInferrer :: TypeInferrer a -> Except String (a, InferrerState)
runTypeInferrer (TypeInferrer inner) = runStateT inner def

evalTypeInferrer :: TypeInferrer a -> Except String a
evalTypeInferrer (TypeInferrer inner) = evalStateT inner def

execTypeInferrer :: TypeInferrer a -> Except String InferrerState
execTypeInferrer (TypeInferrer inner) = execStateT inner def

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
addType :: Id -> Either QualifiedType UninstantiatedQualifiedType -> TypeInferrer ()
addType name t = addTypes (M.singleton name t)

addConstructorType :: Id -> UninstantiatedQualifiedType -> TypeInferrer ()
addConstructorType name t = addType name (Right t)
addFunctionType :: Id -> UninstantiatedQualifiedType -> TypeInferrer ()
addFunctionType name t = addType name (Right t)
addVariableType :: Id -> QualifiedType -> TypeInferrer()
addVariableType name t = addType name (Left t)

addTypePredicates :: S.Set InstantiatedTypePredicate -> TypeInferrer ()
addTypePredicates ps = modify (\s -> s { typePredicates = S.union ps (typePredicates s) })
addTypePredicate :: InstantiatedTypePredicate -> TypeInferrer ()
addTypePredicate = addTypePredicates . S.singleton

getType :: Id -> TypeInferrer (Either QualifiedType UninstantiatedQualifiedType)
getType name = do
    t <- gets (M.lookup name . types)
    maybe (throwError $ printf "Symbol %s not in environment" name) return t

-- These should instantiate the type and add the qualifiers automatically
instantiateConstructor, instantiateFunction :: Id -> TypeInferrer QualifiedType
(instantiateConstructor, instantiateFunction) = (helper "constructor", helper "function")
    where helper s name = do
            t <- getType name >>= either (const $ throwError $ printf "Got instantiated type for %s: %s" s name) return
            t'@(Qualified quals _) <- doInstantiate t
            addTypePredicates quals
            return t'
getVariableType :: Id -> TypeInferrer QualifiedType
getVariableType name = do
        et <- getType name
        either return (const $ throwError $ printf "Got uninstantiated type for variable %s" name) et

-- Return either a variable, function, or constructor. Instantiate and store qualifiers if necessary
getInstantiatedType :: Id -> TypeInferrer QualifiedType
getInstantiatedType name = do
    et <- getType name
    case et of
        Left t -> return t
        Right uninst -> do
            t@(Qualified quals _) <- doInstantiate uninst
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
inferExpression (HsVar name) = do
    -- Get either a function or a variable, instantiating it if necessary
    Qualified _ t <- getInstantiatedType (toId name)
    return t
inferExpression (HsCon name) = do
    -- Instantiate the corresponding constructor
    Qualified _ t <- instantiateConstructor (toId name)
    return t
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, then use unification to deduce the return type
    funType <- inferExpression f
    argType <- inferExpression e
    t <- freshVariable KindStar
    doMatch (makeFun argType t) funType
    s <- get
    traceM ("\nfun: " ++ show funType ++ " arg: " ++ show argType ++ " t: " ++ show t ++ " state: " ++ show s)
    return t
inferExpression (HsInfixApp lhs op rhs) = do
    let opName = case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp (HsVar opName) lhs) rhs)
inferExpression (HsTuple exps) = makeTupleN <$> mapM inferExpression exps
inferExpression e = throwError ("Unsupported expression: " ++ show e)

-- The returned set contains type predicates on the type variables returned in the instantiated type, assumptions
-- mapping variable names in the pattern to their inferred types, and an inferred type for the whole expression
inferPattern :: Syntax.HsPat -> TypeInferrer InstantiatedType
inferPattern (HsPVar name) = do
    t <- freshVariable KindStar
    addVariableType (toId name) (qualifyType t)
    return t
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = freshVariable KindStar
inferPattern (HsPAsPat name pat) = do
    t <- inferPattern pat
    addVariableType (toId name) (qualifyType t)
    return t
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp constructor pats) = do
    -- Infer any nested patterns
    argTypes <- inferPatterns pats
    -- Get the type of the constructor, instantiate it and add the constraints on the new type variables
    Qualified _ constructorType <- instantiateConstructor (toId constructor)
    applicationType <- freshVariable KindStar
    -- Check we have the right number of arguments to the data constructor
    expArgCount <- getKindArgCount <$> getKind constructorType
    let argCount = length argTypes
    when (expArgCount /= argCount) (throwError $ printf "Function expected %d args, got %d" expArgCount argCount)
    -- Unify the expected type with the variables we have to match up their types
    let constructedFnType = foldr makeFun applicationType argTypes -- a -> (b -> (c -> applicationType)))
    unify constructorType constructedFnType
    return applicationType
inferPattern (HsPTuple pats) = makeTupleN <$> inferPatterns pats
-- TODO(kc506): Support more patterns
inferPattern p = throwError ("Unsupported pattern: " ++ show p)

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [InstantiatedType]
inferPatterns = mapM inferPattern


inferAlternative :: [Syntax.HsPat] -> Syntax.HsExp -> TypeInferrer InstantiatedType
inferAlternative pats e = do
    patternTypes <- inferPatterns pats
    expType <- inferExpression e
    -- The "type of the alternative" is the function type that it represents.
    return $ foldr makeFun expType patternTypes

inferAlternatives :: [([Syntax.HsPat], Syntax.HsExp)] -> TypeInferrer InstantiatedType
inferAlternatives alts = do
    types <- mapM (uncurry inferAlternative) alts
    commonType <- freshVariable KindStar
    mapM_ (unify commonType) types
    return commonType

-- |Infers the type of a pattern binding (eg. `foo = 5`) without an explicit type
inferImplicitPatternBinding :: Syntax.HsPat -> Syntax.HsRhs -> TypeInferrer ()
inferImplicitPatternBinding pat (HsUnGuardedRhs e) = do
    sub <- getSubstitution
    patType <- applySub sub <$> inferPattern pat
    rhsType <- applySub sub <$> inferExpression e
    unify patType rhsType
    classEnv <- getClassEnvironment
    preds <- applySub sub <$> getPredicates
    let rhsTypeVariables = S.fromList $ getTypeVars $ applySub sub rhsType
    (qualifiers, _) <- split classEnv rhsTypeVariables preds
    -- TODO(kc506): Set the set of assumptions in the state to be exactly the deferred assumptions?
    --mapM_ (uncurry addVariableType)
    return ()
inferImplicitPatternBinding _ (HsGuardedRhss _) = throwError "Guarded patterns aren't yet supported"