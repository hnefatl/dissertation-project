module Typechecker.TypeclassesSpec where

import           BasicPrelude
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCase)

import           Data.Text               (unpack)
import           Logger                  (runLoggerT)
import           NameGenerator           (evalNameGenerator)
import           Names                   (TypeVariableName(..))
import           TextShow                (showt)
import           Typechecker.Hardcoded
import           Typechecker.Typechecker (evalTypeInferrer)
import           Typechecker.Typeclasses
import           Typechecker.Types

import           Control.Monad           ()
import           Control.Monad.Except    (runExceptT)
import qualified Data.Set                as S

makeTest :: ClassEnvironment -> S.Set TypePredicate -> TypePredicate -> TestTree
makeTest ce ps goal = testCase (unpack $ showt goal) $ case result of
        Left err      -> assertFailure $ unpack $ unlines [err, "Logs:", unlines logs]
        Right success -> if success then return () else assertFailure $ unpack $ "Failed to prove.\n" <> unlines logs
    where (result, logs) = evalNameGenerator (runLoggerT $ runExceptT $ evalTypeInferrer $ entails ce ps goal) 0

makeFailTest :: ClassEnvironment -> S.Set TypePredicate -> TypePredicate -> TestTree
makeFailTest ce ps goal = testCase (unpack $ "Fails: " <> showt goal) $ case result of
        Left _        -> return ()
        Right success -> when success (assertFailure $ unpack $ "Succeeded in proving. Logs:\n" <> unlines logs)
    where (result, logs) = evalNameGenerator (runLoggerT $ runExceptT $ evalTypeInferrer $ entails ce ps goal) 0

test :: TestTree
test = testGroup "Type Classes"
    [
        makeTest builtinClasses S.empty (IsInstance (TypeVariableName "Eq") typeBool),
        makeTest builtinClasses S.empty (IsInstance (TypeVariableName "Eq") typeString),
        makeFailTest builtinClasses S.empty (IsInstance (TypeVariableName "Eq") typeFloat)
    ]
