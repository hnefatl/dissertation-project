module Typechecker.TypecheckerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Typechecker.Types
import Typechecker.Unifier
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

testBindings :: String -> [(Id, QualifiedType)] -> TestTree
testBindings s cases = testCase s $ do
    let HsModule _ _ _ _ decls = parse s
        bindings = [ (pat, rhs) | (HsPatBind _ pat rhs _) <- decls ]
    state <- unpackEither $ runExcept $ execTypeInferrer $ do
        addClasses builtinClasses
        forM_ (M.toList builtinConstructors) (uncurry addConstructorType)
        mapM_ (uncurry inferImplicitPatternBinding) bindings
    let (types', _) = M.mapEither id (types state)
        -- Remove ambiguity by specifying types explicitly
        alphaEq' :: Maybe UninstantiatedQualifiedType -> Maybe UninstantiatedQualifiedType -> Bool
        alphaEq' = alphaEq
        check (name, t) = assertBool s $ alphaEq' (uninstantiate $ Just t) (uninstantiate $ M.lookup name types')
    mapM_ check cases

unpackEither :: Either String b -> IO b
unpackEither = either error return

test :: TestTree
test = testGroup "Typechecking"
    [
        let s = "x = 5"
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ,
        let s = "x = 'a'"
        in testBindings s [("x", Qualified S.empty typeChar)]
    ,
        let s = "x = \"ab\""
        in testBindings s [("x", Qualified S.empty typeString)]
    ,
        let s = "x = True"
        in testBindings s [("x", Qualified S.empty typeBool)]
    ,
        let s = "x = False"
        in testBindings s [("x", Qualified S.empty typeBool)]
    ,
        let s = "(x, y) = (1, True)" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t), ("y", Qualified S.empty typeBool)]
    ,
        let s = "(x, (y, z, w)) = (1, (True, False, \"Hi\"))" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t), ("y", Qualified S.empty typeBool), ("z", Qualified S.empty typeBool), ("w", Qualified S.empty typeString)]
    ,
        let s = "x = (+) 3 4" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ,
        let s = "x = 1 + 2" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ,
        let s = "x = 1 + 2 + 3" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ]