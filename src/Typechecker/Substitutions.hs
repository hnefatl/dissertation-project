module Typechecker.Substitutions where

import qualified Data.Map as M
import Typechecker.Types (TypeVariable, Type)

-- A substitution is a collection of assignments of a type to a variable
type Substitution = M.Map TypeVariable Type

