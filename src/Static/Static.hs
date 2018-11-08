{-# Language FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving #-}

module Static.Static where

import Language.Haskell.Syntax
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

import ExtraDefs

data StaticState = StaticState
    { variableMap :: M.Map Id Id
    , variableCounter :: Int }

newtype Static a = Static (ExceptT String (State StaticState) a)
    deriving (Functor, Applicative, Monad, MonadState StaticState, MonadError String)
instance NameGenerator Static where
    freshName = do
        counter <- (1 +) <$> gets variableCounter
        modify (\s -> s { variableCounter = counter })
        return ("v" ++ show counter)

-- |Execute an action in a nested scope, discarding certain scope-sensitive changes to the state.
nestScope :: MonadState StaticState m => m a -> m a
nestScope action = do
    oldMap <- gets variableMap
    result <- action
    modify (\s -> s { variableMap = oldMap })
    return result

-- Need state from existing variables to new names
-- Need way more finesse: need scoping rules on the state, eg. `f [x, a] = x + a`.
processModule :: HsModule -> Static HsModule
processModule (HsModule a b c d decls) = HsModule a b c d <$> mapM (nestScope . processDecl) decls

processDecl :: HsDecl -> Static HsDecl
processDecl (HsClassDecl a b c d decls) = HsClassDecl a b c d <$> mapM processDecl decls
processDecl (HsInstDecl a b c d decls) = HsInstDecl a b c d <$> mapM processDecl decls
processDecl (HsPatBind a b rhs decls) = HsPatBind a b <$> processRhs rhs <*> mapM processDecl decls
processDecl (HsFunBind matches) = HsFunBind <$> mapM processMatch matches
processDecl x = return x

processRhs :: HsRhs -> Static HsRhs
processRhs (HsUnGuardedRhs exp) = HsUnGuardedRhs <$> processExp exp
processRhs (HsGuardedRhss rhss) = HsGuardedRhss <$> mapM processRhs rhss

processMatch :: HsMatch -> Static HsMatch
processMatch (HsMatch a b ) = undefined