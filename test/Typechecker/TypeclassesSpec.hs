module Typechecker.TypeclassesSpec where

import qualified Data.Set as S

import Test.Tasty
import Test.Tasty.HUnit

import Typechecker.Typechecker
import Typechecker.Hardcoded
import Typechecker.Typeclasses
import Typechecker.Types

makeTest :: ClassEnvironment -> S.Set InstantiatedTypePredicate -> InstantiatedTypePredicate -> TestTree
makeTest ce ps goal = testCase (show goal) $ case result of
        Left err -> assertFailure err
        Right success -> if success then return () else assertFailure "Failed to prove"
    where result = evalTypeInferrer $ entails ce ps goal

makeFailTest :: ClassEnvironment -> S.Set InstantiatedTypePredicate -> InstantiatedTypePredicate -> TestTree
makeFailTest ce ps goal = testCase ("Fails: " ++ show goal) $ case result of
        Left _ -> return ()
        Right success -> if success then assertFailure "Succeeded in proving" else return ()
    where result = evalTypeInferrer $ entails ce ps goal

a, b :: UninstantiatedType
a = TypeVar (TypeDummy "a" KindStar)
b = TypeVar (TypeDummy "a" KindStar)

test :: TestTree
test = testGroup "Typeclasses"
    [ 
        makeTest builtinClasses S.empty (IsInstance "Eq" typeBool),
        makeTest builtinClasses S.empty (IsInstance "Eq" typeString),
        makeFailTest builtinClasses S.empty (IsInstance "Eq" typeFloat)
    ]