{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Preprocessor.Dependency where

import           BasicPrelude
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Extra         (concatForM)
import           Data.Graph                  (flattenSCC, stronglyConnComp)
import           Data.List                   (nub)
import qualified Data.Set                    as S
import           Language.Haskell.Syntax
import           TextShow                    (showt)
import           TextShow.Instances          ()

import           Logger                      (MonadLogger, writeLog)
import           Names                       (VariableName, TypeVariableName)
import           NameGenerator               (MonadNameGenerator, freshVarName)
import           Preprocessor.ContainedNames

variableUnion :: S.Set VariableName -> S.Set TypeVariableName -> S.Set (Either VariableName TypeVariableName)
variableUnion vs tvs = S.union (S.map Left vs) (S.map Right tvs)

dependencyOrder :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => [HsDecl] -> m [[HsDecl]]
dependencyOrder ds = do
    -- We do dependency analysis taking into account bound variables/type variables, as we need to compile eg.
    -- `data Bool = False | True` before `idBool :: Bool -> Bool`.
    declBindings <- variableUnion <$> getBoundVariables ds <*> pure (getBoundTypeConstants ds)
    writeLog $ "declBindings: " <> showt declBindings
    adjacencyList <- concatForM ds $ \d -> do
        -- Names bound by this declaration
        boundVars <- variableUnion <$> getBoundVariables d <*> pure (getBoundTypeConstants d)
        -- If this is a declaration that doesn't actually bind any variables, we want to add it as a node that can't
        -- be depended on but can depend on others: this is an exceptional case. We generate a fresh unique variable
        -- name and pretend this declaration binds that variable.
        boundVars' <- if S.null boundVars then S.singleton . Left <$> freshVarName else return boundVars
        writeLog $ "boundVars': " <> showt boundVars'
        -- Any variables used in this declaration that are also defined in this binding group
        freeVars <- variableUnion <$> getFreeVariables d <*> pure (getFreeTypeConstants d)
        writeLog $ "freeVars: " <> showt freeVars
        let contained = S.intersection declBindings freeVars
        writeLog $ "contained: " <> showt contained
        -- We make one node for each bound variable: the "node" is the declaration binding that variable, the
        -- key is the variable itself, and the dependencies are all variables used in the declaration along with
        -- all other variables bound by the declaration: otherwise vars bound in the same decl are seen as
        -- independent.
        return $ map (d,, S.toList $ S.union contained boundVars') (S.toList boundVars')
    let sccs = stronglyConnComp adjacencyList
    return $ map (nub . flattenSCC) sccs
