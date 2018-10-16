{-# Language FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase #-}

module Typechecker.Typechecker where

import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import ExtraDefs

import Debug.Trace

import Data.Default
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M

import Language.Haskell.Syntax as Syntax

-- |Assumptions on the type of a variable
type Assumptions = M.Map Id UninstantiatedQualifiedType

-- |An inferrer carries along a working substitution of type variable names, a global variable counter for making new
-- unique variable names
data InferrerState = InferrerState { subs :: Substitution , variableCounter :: Int }
instance Default InferrerState where
    def = InferrerState { subs = def, variableCounter = 1 }

-- |A TypeInferrer handles mutable state, some scoped read-only assumptions about the types of variables, and error
-- reporting
newtype TypeInferrer a = TypeInferrer (ReaderT Assumptions (StateT InferrerState (Except String) ) a)
    deriving (Functor, Applicative, Monad, MonadReader Assumptions, MonadState InferrerState, MonadError String)

runTypeInferrer :: TypeInferrer a -> Except String a
runTypeInferrer (TypeInferrer inner) = evalStateT (runReaderT inner M.empty) def

instance TypeInstantiator TypeInferrer where
    freshName = do
        counter <- (1 +) <$> gets variableCounter
        modify (\s -> s { variableCounter = counter })
        return ("v" ++ show counter)

-- |Returns the current substitution in the monad
getSubstitution :: TypeInferrer Substitution
getSubstitution = gets subs

-- |Creates a fresh (uniquely named) type
freshVariable :: Kind -> TypeInferrer InstantiatedType
freshVariable kind = freshName >>= \name -> return $ TypeVar (TypeVariable name kind)

-- |Runs type inference with updated assumptions
withAssumptions :: Assumptions -> TypeInferrer a -> TypeInferrer a
withAssumptions as = local (M.union as)

getAssumption :: Id -> TypeInferrer UninstantiatedQualifiedType
getAssumption name = reader (M.lookup name) >>= \case
    Just t -> return t
    Nothing -> throwError ("Assumption " ++ name ++ " not in environment" )

-- |Extend the current substitution with an mgu that unifies the two arguments
unify :: InstantiatedType -> InstantiatedType -> TypeInferrer ()
unify t1 t2 = do
    currentSub <- getSubstitution
    newSub <- mgu (applySub currentSub t1) (applySub currentSub t2)
    modify (\s -> s { subs = subCompose currentSub newSub })


inferLiteral :: Syntax.HsLiteral -> TypeInferrer (S.Set InstantiatedTypePredicate, InstantiatedType)
inferLiteral (HsChar _) = return (S.empty, typeChar)
inferLiteral (HsString _) = return (S.empty, typeString)
inferLiteral (HsInt _) = freshVariable KindStar >>= \v -> return (S.singleton $ IsInstance "Num" v, v)
inferLiteral (HsFrac _) = freshVariable KindStar >>= \v -> return (S.singleton $ IsInstance "Fractional" v, v)
inferLiteral l = throwError ("Unboxed literals not supported: " ++ show l)

-- TODO(kc506): Change from using the raw syntax expressions to using an intermediate form with eg. lambdas converted
-- into lets? Pg. 26 of thih.pdf
inferExpression :: Syntax.HsExp -> TypeInferrer (S.Set InstantiatedTypePredicate, InstantiatedType)
inferExpression (HsVar name) = do
    uninstType <- getAssumption (toId name)
    Qualified quals t <- doInstantiate uninstType
    return (quals, t)
inferExpression (HsCon name) = inferExpression (HsVar name) -- We treat variables and type constructors the same here
inferExpression (HsLit literal) = inferLiteral literal
inferExpression (HsApp f e) = do
    (preds1, funType) <- inferExpression f
    (preds2, argType) <- inferExpression e
    t <- freshVariable KindStar
    unify funType (makeFun argType t)
    s <- getSubstitution
    traceM ("\nfun: " ++ show funType ++ " arg: " ++ show argType ++ " t: " ++ show t ++ " subs: " ++ show s ++ " preds1: " ++ show preds1 ++ " preds2: " ++ show preds2)
    return (S.union preds1 preds2, t)
inferExpression e = throwError ("Unsupported expression: " ++ show e)