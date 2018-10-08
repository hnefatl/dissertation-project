module Typechecker.TypeScheme where

import Typechecker.Types
import Typechecker.Typeclasses

-- A type scheme is a qualified type with free type variables quantified
-- Eg. `forall a . a -> a -> Bool`
data TypeScheme = Forall [Kind] ClassInstance