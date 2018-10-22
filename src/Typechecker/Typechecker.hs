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
getInstantiatedType name = getType name >>= \case
    Left t -> return t
    Right uninst -> do
        t@(Qualified quals _) <- doInstantiate uninst
        addTypePredicates quals
        return t

qualifyType :: S.Set InstantiatedTypePredicate -> InstantiatedType -> TypeInferrer QualifiedType
qualifyType ps t = do
    addTypePredicates ps
    return (Qualified ps t)

mergeContexts :: [S.Set InstantiatedTypePredicate] -> TypeInferrer (S.Set InstantiatedTypePredicate)
mergeContexts preds = do
    sub <- getSubstitution
    classEnv <- getClassEnvironment
    simplify classEnv $ S.unions $ map (applySub sub) preds
        
unzipQualifieds :: [QualifiedType] -> ([S.Set InstantiatedTypePredicate], [InstantiatedType])
unzipQualifieds = unzip . map (\(Qualified qs ts) -> (qs, ts))


-- |Extend the current substitution with an mgu that unifies the two arguments
-- |Same as unify but only unifying variables in the first argument with those in the left
unify, doMatch :: InstantiatedType -> InstantiatedType -> TypeInferrer ()
(unify, doMatch) = (helper mgu, helper match)
    where helper f t1 t2 = do
            currentSub <- getSubstitution
            newSub <- f (applySub currentSub t1) (applySub currentSub t2)
            modify (\s -> s { subs = subCompose currentSub newSub })


inferLiteral :: Syntax.HsLiteral -> TypeInferrer QualifiedType
inferLiteral (HsChar _) = return (Qualified S.empty typeChar)
inferLiteral (HsString _) = return (Qualified S.empty typeString)
inferLiteral (HsInt _) = do
    v <- freshVariable KindStar
    qualifyType (S.singleton $ IsInstance "Num" v) v
inferLiteral (HsFrac _) = do
    v <- freshVariable KindStar
    qualifyType (S.singleton $ IsInstance "Fractional" v) v
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

-- TODO(kc506): Change from using the raw syntax expressions to using an intermediate form with eg. lambdas converted
-- into lets? Pg. 26 of thih.pdf
inferExpression :: Syntax.HsExp -> TypeInferrer QualifiedType
inferExpression (HsVar name) = getInstantiatedType (toId name)
inferExpression (HsCon name) = instantiateConstructor (toId name)
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsParen e) = inferExpression e
inferExpression (HsApp f e) = do
    -- Infer the function's type and the expression's type, then use unification to deduce the return type
    Qualified funquals funtype <- inferExpression f
    Qualified argquals argtype <- inferExpression e
    t <- freshVariable KindStar
    unify (makeFun [argtype] t) funtype
    Qualified <$> mergeContexts [funquals, argquals] <*> pure t
inferExpression (HsInfixApp lhs op rhs) = do
    let opName = case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    -- Translate eg. `x + y` to `(+) x y` and `x \`foo\` y` to `(foo) x y`
    inferExpression (HsApp (HsApp (HsVar opName) lhs) rhs)
inferExpression (HsTuple exps) = do
    (ps, ts) <- unzipQualifieds <$> mapM inferExpression exps
    Qualified <$> mergeContexts ps <*> pure (makeTuple ts)
inferExpression (HsLambda _ pats e) = do
    (argQuals, argTypes) <- unzipQualifieds <$> inferPatterns pats
    Qualified bodyQuals bodyType <- inferExpression e
    Qualified <$> mergeContexts (bodyQuals:argQuals) <*> pure (makeFun argTypes bodyType)
inferExpression (HsLet decls e) = do
    mapM_ inferDecl decls
    inferExpression e
inferExpression (HsIf c e1 e2) = undefined
inferExpression e = throwError ("Unsupported expression: " ++ show e)


inferPattern :: Syntax.HsPat -> TypeInferrer QualifiedType
inferPattern (HsPVar name) = do
    t <- Qualified S.empty <$> freshVariable KindStar
    addVariableType (toId name) t
    return t
inferPattern (HsPLit lit) = inferLiteral lit
inferPattern HsPWildCard = Qualified S.empty <$> freshVariable KindStar
inferPattern (HsPAsPat name pat) = do
    t <- inferPattern pat
    addVariableType (toId name) t
    return t
inferPattern (HsPParen pat) = inferPattern pat
inferPattern (HsPApp constructor pats) = do
    -- Infer any nested patterns
    (argQuals, argTypes) <- unzipQualifieds <$> inferPatterns pats
    -- Get the type of the constructor, instantiate it and add the constraints on the new type variables
    Qualified conQuals constructorType <- instantiateConstructor (toId constructor)
    -- Check we have the right number of arguments to the data constructor
    let expArgCount = getKindArgCount $ getKind constructorType
        argCount = length argTypes
    when (expArgCount /= argCount) (throwError $ printf "Function expected %d args, got %d" expArgCount argCount)
    -- Unify the expected type with the variables we have to match up their types
    returnType <- freshVariable KindStar
    let constructedFnType = makeFun argTypes returnType
    unify constructorType constructedFnType
    Qualified <$> mergeContexts (conQuals:argQuals) <*> pure returnType
inferPattern (HsPTuple pats) = do
    (qs, ts) <- unzipQualifieds <$> inferPatterns pats
    Qualified <$> mergeContexts qs <*> pure (makeTuple ts)
-- TODO(kc506): Support more patterns
inferPattern p = throwError ("Unsupported pattern: " ++ show p)

inferPatterns :: [Syntax.HsPat] -> TypeInferrer [QualifiedType]
inferPatterns = mapM inferPattern


-- TODO(kc506): Change the return type to just yield all the constituent qualified types.
-- Might make more useful if this is going to be used for function alternatives as well as case statements, etc.
inferAlternative :: [Syntax.HsPat] -> Syntax.HsExp -> TypeInferrer QualifiedType
inferAlternative pats e = do
    (patQuals, patTypes) <- unzipQualifieds <$> inferPatterns pats
    Qualified rhsQuals rhsType <- inferExpression e
    -- The "type of the alternative" is the function type that it represents.
    Qualified <$> mergeContexts (rhsQuals:patQuals) <*> pure (makeFun patTypes rhsType)

inferAlternatives :: [([Syntax.HsPat], Syntax.HsExp)] -> TypeInferrer QualifiedType
inferAlternatives alts = do
    (qs, ts) <- unzipQualifieds <$> mapM (uncurry inferAlternative) alts
    commonType <- freshVariable KindStar
    mapM_ (unify commonType) ts
    Qualified <$> mergeContexts qs <*> pure commonType

-- |Infers the type of a pattern binding (eg. `foo = 5`) without an explicit type
inferImplicitPatternBinding :: Syntax.HsPat -> Syntax.HsRhs -> TypeInferrer ()
inferImplicitPatternBinding pat (HsUnGuardedRhs e) = do
    Qualified patquals pattype <- inferPattern pat
    Qualified rhsquals rhstype <- inferExpression e
    unify pattype rhstype
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