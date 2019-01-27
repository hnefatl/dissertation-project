{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Preprocessor.Dependency where

import           BasicPrelude
import           Control.Monad.Except        (MonadError)
import           Control.Monad.Extra         (concatForM)
import           Data.Graph                  (flattenSCC, stronglyConnComp)
import           Data.List                   (nub)
import           Data.Text.Lazy              (toStrict)
import           Text.Pretty.Simple          (pShow)
import qualified Data.Set                    as S
import           Language.Haskell.Syntax
import           TextShow                    (showt)
import           TextShow.Instances          ()

import           Logger                      (MonadLogger, writeLog)
import           Names                       (VariableName, TypeVariableName)
import           NameGenerator               (MonadNameGenerator, freshVarName)
import           Preprocessor.ContainedNames

-- |Keep type variable names (`Num`, `Eq`) distinct from variable names (`x`, `(+)`).
variableUnion :: S.Set VariableName -> S.Set TypeVariableName -> S.Set (Either VariableName TypeVariableName)
variableUnion vs tvs = S.union (S.map Left vs) (S.map Right tvs)

-- For dependency analysis, we pretend instances don't bind any variables: instead, their member symbols are free. This
-- allows their dependencies and dependents to be calculated correctly, but doesn't really match the conventional
-- bound/free definitions. These functions wrap the conventional getBound/Free functions to match our contextual
-- definition.

getDepBoundVariables :: (MonadNameGenerator m, MonadError Text m) => HsDecl -> m (S.Set (Either VariableName TypeVariableName))
getDepBoundVariables HsInstDecl{} = return S.empty 
getDepBoundVariables d = variableUnion <$> getBoundVariables d <*> pure (getBoundTypeConstants d)

getDepFreeVariables :: (MonadNameGenerator m, MonadError Text m) => HsDecl -> m (S.Set (Either VariableName TypeVariableName))
getDepFreeVariables d@HsInstDecl{} = do
    actualFree <- variableUnion <$> getFreeVariables d <*> pure (getFreeTypeConstants d)
    fakeFree <- variableUnion <$> getBoundVariables d <*> pure (getBoundTypeConstants d)
    return $ S.union actualFree fakeFree
getDepFreeVariables d = variableUnion <$> getFreeVariables d <*> pure (getFreeTypeConstants d)

dependencyOrder :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => [HsDecl] -> m [[HsDecl]]
dependencyOrder ds = do
    -- We do dependency analysis taking into account bound variables/type variables, as we need to compile eg.
    -- `data Bool = False | True` before `idBool :: Bool -> Bool`.
    declBindings <- S.unions <$> mapM getDepBoundVariables ds
    writeLog $ "declBindings: " <> showt declBindings
    adjacencyList <- concatForM ds $ \d -> do
        -- Names bound by this declaration
        boundVars <- getDepBoundVariables d
        -- If this is a declaration that doesn't actually bind any variables, we want to add it as a node that can't
        -- be depended on but can depend on others: this is an exceptional case. We generate a fresh unique variable
        -- name and pretend this declaration binds that variable.
        boundVars' <- if S.null boundVars then S.singleton . Left <$> freshVarName else return boundVars
        writeLog $ "boundVars': " <> showt boundVars'
        -- Any variables used in this declaration that are also defined in this binding group
        freeVars <- getDepFreeVariables d
        writeLog $ "freeVars: " <> showt freeVars
        let contained = S.intersection declBindings freeVars
        writeLog $ "contained: " <> showt contained
        -- We make one node for each bound variable: the "node" is the declaration binding that variable, the
        -- key is the variable itself, and the dependencies are all variables used in the declaration along with
        -- all other variables bound by the declaration: otherwise vars bound in the same decl are seen as
        -- independent.
        return $ map (d,, S.toList $ S.union contained boundVars') (S.toList boundVars')
    writeLog $ unlines ["adjacencyList:", toStrict $ pShow adjacencyList]
    let sccs = stronglyConnComp adjacencyList
    writeLog $ unlines ["SCCs:", toStrict $ pShow sccs]
    return $ map (nub . flattenSCC) sccs
