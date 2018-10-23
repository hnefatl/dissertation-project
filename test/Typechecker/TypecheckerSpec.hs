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
    addClasses builtinClasses
    forM_ (M.toList builtinConstructors) (uncurry addConstructorType)
    mapM_ (uncurry inferImplicitPatternBinding) bindings
    where HsModule _ _ _ _ decls = parse s
          bindings = [ (pat, rhs) | (HsPatBind _ pat rhs _) <- decls ]

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
        -- Simple literal type checks
        let s = "x = 5"
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ,
        let s = "x = 1.2"
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Fractional" t) t)]
    ,
        let s = "x = 'a'"
        in testBindings s [("x", Qualified S.empty typeChar)]
    ,
        let s = "x = \"ab\""
        in testBindings s [("x", Qualified S.empty typeString)]
    ,
        testBindingsFail "x = \"hi"
    ,
        testBindingsFail "x = 'hi"
    ,
        -- Data constructors
        let s = "x = True"
        in testBindings s [("x", Qualified S.empty typeBool)]
    ,
        let s = "x = False"
        in testBindings s [("x", Qualified S.empty typeBool)]
    ,
        testBindingsFail "x = Foo"
    ,
        -- Pattern matching
        let s = "(x, y) = (1, True)" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t), ("y", Qualified S.empty typeBool)]
    ,
        let s = "(x, _, _) = (True, False, True)"
        in testBindings s [("x", Qualified S.empty typeBool)]
    ,
        let s = "a@(x, _, _) = (True, False, True)"
            t = makeTuple (replicate 3 typeBool)
        in testBindings s [("x", Qualified S.empty typeBool), ("a", Qualified S.empty t)]
    ,
        let s = "a@(_, y) = (1, True)"
            v = TypeVar (TypeVariable "a" KindStar)
            t = makeTuple [v, typeBool]
        in testBindings s [("y", Qualified S.empty typeBool), ("a", Qualified (S.singleton $ IsInstance "Num" t) t)]
    ,
        let s = "(x, y) = (1, (True))"
            t = TypeVar (TypeVariable "a" KindStar)
            q = (S.singleton $ IsInstance "Num" t)
        in testBindings s [("x", Qualified q t), ("y", Qualified q (makeTuple [t, typeBool]))]
    ,
        testBindingsFail "(x, y) = True" 
    ,
        let s = "(x, (y, z, w)) = (1, (True, False, \"Hi\"))" 
            t = TypeVar (TypeVariable "a" KindStar)
        in testBindings s [("x", Qualified (S.singleton $ IsInstance "Num" t) t), ("y", Qualified S.empty typeBool), ("z", Qualified S.empty typeBool), ("w", Qualified S.empty typeString)]
        -- TODO(kc506): Test pattern matching with data constructors
    ,
        -- Function application (prefix and infix)
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
        let s = "x = 1 + 2\ny = x + 3"
            t = TypeVar (TypeVariable "a" KindStar)
            q = Qualified (S.singleton $ IsInstance "Num" t) t
        in testBindings s [("x", q), ("y", q)]
    ,
        -- TODO(kc506): This doesn't fail as `Num Bool` doesn't fail - check paper for where it should
    --     getQualifiedTypeFrom typechecker state? Revert from qualified types and make getting the qualified version an
    --     explicit operation?
        testBindingsFail "x = 1 && True"
    ,
        -- Lambdas
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
    ,
        let s = "y = let f = \\x -> x in f 5 + f 6"
            a = TypeVar (TypeVariable "a" KindStar)
            b = TypeVar (TypeVariable "b" KindStar)
        in testBindings s [("y", Qualified (S.singleton $ IsInstance "Num" b) b), ("f", Qualified (S.singleton $ IsInstance "Num" a) a)]
    ,
        let s = "y = let f = \\x -> x\n" ++
                "        g = \\x y -> y\n" ++
                "    in g (f 5) (f True)"
        in testBindings s [("y", Qualified S.empty typeBool)]
    ]