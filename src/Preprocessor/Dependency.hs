{-# Language TupleSections, FlexibleContexts #-}

module Preprocessor.Dependency where

import Data.Graph
import qualified Data.Set as S
import Data.List (nub)
import Control.Monad.Except
import Language.Haskell.Syntax

import Preprocessor.ContainedNames

dependencyOrder :: MonadError String m => [HsDecl] -> m [[HsDecl]]
dependencyOrder ds = do
    declBindings <- getDeclsBoundNames ds -- The names bound in this declaration group
    let declToNodes d = do
            bound <- S.toList <$> getDeclBoundNames d -- All names bound by this declaration
            -- Any variables used in this declaration that are also defined in this binding group
            contained <- S.intersection declBindings <$> getDeclContainedNames d 
            return $ map (\n -> (d, n, S.toList contained)) bound
    adjacencyList <- concat <$> mapM declToNodes ds
    let sccs = stronglyConnComp adjacencyList
    return $ map (nub . flattenSCC) sccs