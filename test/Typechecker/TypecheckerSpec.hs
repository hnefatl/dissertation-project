module Typechecker.TypecheckerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Typechecker.Types
import Typechecker.Typechecker
import Typechecker.Hardcoded

import Data.Foldable
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Map as M

parse :: String -> HsModule
parse s = case parseModule s of
    ParseOk m -> m
    (ParseFailed loc msg) -> error (msg ++ ": " ++ show loc)

parseExpression :: String -> HsExp
parseExpression s = head $ map (\(HsPatBind _ _ (HsUnGuardedRhs e) _) -> e) decls
    where (HsModule _ _ _ _ decls) = parse s

-- TODO(kc506): Will need updating. Replace this with some framework based around testing implicitly typed bindings,
-- using qualified types which include the type constraints and not just the type variable names
testExpression :: String -> (S.Set InstantiatedTypePredicate, InstantiatedType) -> TestTree
testExpression s (_, expectedType) = testCase s $ do
    actualType <- unpackEither $ runExcept $ runTypeInferrer $ do
        forM_ (M.toList builtinConstructors) (uncurry addConstructorType)
        inferExpression (parseExpression s)
    assertEqual s expectedType actualType

unpackEither :: Either String b -> IO b
unpackEither = either error return

test :: TestTree
test = testGroup "Typechecking"
    [
        let s = "x = 5"
            t = TypeVar (TypeVariable "a" KindStar)
            expected = (S.singleton (IsInstance "Num" t), t)
        in testExpression s expected
    ,
        let s = "x = 'a'"
            expected = (S.empty, typeChar)
        in testExpression s expected
    ,
        let s = "x = \"ab\""
            expected = (S.empty, typeString)
        in testExpression s expected
    ,
        let s = "x = True"
            expected = (S.empty, typeBool)
        in testExpression s expected
    ,
        let s = "x = False"
            expected = (S.empty, typeBool)
        in testExpression s expected
    ,
        let s = "x = (+) 3 4" 
            t = TypeVar (TypeVariable "a" KindStar)
            expected = (S.singleton (IsInstance "Num" t), t)
        in testExpression s expected
    ]