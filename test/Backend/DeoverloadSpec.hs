module Backend.DeoverloadSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Names
import Typechecker.Types
import Typechecker.Typechecker
import Backend.Deoverload

import Text.Printf

test :: TestTree
test = testGroup "Deoverload"
    [  ]