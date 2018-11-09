{-# Language GeneralizedNewtypeDeriving #-}

module Static.Renamer where

import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.List

import ExtraDefs

type RenameMap = M.HashMap Id Id
type TreePath = [Int]

data RenamerState = RenamerState
    { variableCounter :: Int
    , bindings :: M.HashMap TreePath RenameMap }

-- |Get the map from variable to unique variable name, constructed by walking up the path and unioning the bindings.
-- As M.unions is left-biased, we reverse the prefixes so priority is given to the nearest binding.
getRenameMap :: TreePath -> Renamer RenameMap
getRenameMap path = M.unions <$> mapM (\k -> gets (M.lookupDefault M.empty k . bindings)) prefixes
    where prefixes = reverse (inits path) -- Prefixes in decreasing length order, eg. [[1,2,3], [1,2], [1]]

newtype Renamer a = Deduper (State RenamerState a)
    deriving (Applicative, Functor, Monad, MonadState RenamerState)