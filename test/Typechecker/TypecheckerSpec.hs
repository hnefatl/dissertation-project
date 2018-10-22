module Typechecker.TypecheckerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Typechecker
import Typechecker.Hardcoded

import Data.List
import Data.Foldable
import Data.Either
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Map as M

parse :: String -> HsModule
parse s = case parseModule s of
    ParseOk m -> m
    (ParseFailed loc msg) -> error (msg ++ ": " ++ show loc)

deline :: String -> String
deline = intercalate " \\ne " . lines

inferModule :: String -> Either String InferrerState
inferModule s = runExcept $ execTypeInferrer $ do
    -- Add builtins
    addClasses builtinClasses
    forM_ (M.toList builtinConstructors) (uncurry addConstructorType)
    forM_ (M.toList builtinFunctions) (uncurry addFunctionType)
    -- Parse and run type inference
    let HsModule _ _ _ _ decls = parse s
    mapM_ inferDecl decls
        

testBindings :: String -> [(Id, QualifiedType)] -> TestTree
testBindings s cases = testCase (deline s) $ do
    state <- unpackEither $ inferModule s
    let (ts, _) = M.mapEither id (types state)
        -- Remove ambiguity by specifying types explicitly
        alphaEq' :: Maybe UninstantiatedQualifiedType -> Maybe UninstantiatedQualifiedType -> Bool
        alphaEq' = alphaEq
        check (name,t) = assertBool (deline s) $ alphaEq' (uninstantiate $ Just t) (uninstantiate $ M.lookup name ts)
    mapM_ check cases

testBindingsFail :: String -> TestTree
testBindingsFail s = testCase ("Fails: " ++ s') $ assertBool (s' ++ ": " ++ show state) (isLeft state)
    where s' = deline s
          state = inferModule s

unpackEither :: Either String b -> IO b
unpackEither = either assertFailure return

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
    ,
        let s = "(x, y) = (1, (True))"
            t = TypeVar (TypeVariable "a" KindStar)
            q = (S.singleton $ IsInstance "Num" t)
        in testBindings s [("x", Qualified q t), ("y", Qualified q (makeTuple [t, typeBool]))]
    ,
        testBindingsFail "(x, y) = True" 
    ,
        testBindingsFail "x = (+) 1 2 3"
    ,
      let s = "x = 1 + 2\ny = x + 3"
          t = TypeVar (TypeVariable "a" KindStar)
          q = Qualified (S.singleton $ IsInstance "Num" t) t
      in testBindings s [("x", q), ("y", q)]
    ,
      let s = "x = (\\y -> 1 + y)"
          t = TypeVar (TypeVariable "a" KindStar)
          q = S.singleton $ IsInstance "Num" t
      in testBindings s [("y", Qualified q t), ("x", Qualified q (makeFun [t] t))]
    ,
      let s = "x = (\\y -> False && y)"
      in testBindings s [("y", Qualified S.empty typeBool), ("x", Qualified S.empty (makeFun [typeBool] typeBool))]
    ,
      let s = "x = (\\f -> f True)"
          t = TypeVar (TypeVariable "a" KindStar)
      in testBindings s [("x", Qualified S.empty (makeFun [makeFun [typeBool] t] t))]
    ,
      let s = "x = (\\f -> f True) (\\y -> not (not y))"
      in testBindings s [("x", Qualified S.empty typeBool)]
    ]