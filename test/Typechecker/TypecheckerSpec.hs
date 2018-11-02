{-# Language FlexibleContexts #-}

module Typechecker.TypecheckerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Substitution
import Typechecker.Typechecker
import Typechecker.Substitution
import Typechecker.Hardcoded

import Data.List
import Data.Foldable
import Data.Either
import Data.Text.Lazy (unpack)
import Text.Printf
import Text.Pretty.Simple
import Debug.Trace
import Control.Monad.State.Strict (get)
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Map as M

parse :: MonadError String m => String -> m HsModule
parse s = case parseModule s of
    ParseOk m -> return m
    (ParseFailed loc msg) -> throwError (msg ++ ": " ++ show loc)

deline :: String -> String
deline = intercalate " \\n " . lines

inferModule :: String -> (Either String (M.Map Id QuantifiedType), InferrerState)
inferModule s = (runExcept out, state)
    where
        (out, state) = runTypeInferrer $ catchError infer handler
        handler err = do
            state <- get
            throwError $ unlines [err, unpack $ pShow state]
        infer = do
            -- Add builtins
            addClasses builtinClasses
            forM_ (M.toList builtinConstructors ++ M.toList builtinFunctions) (uncurry insertQuantifiedType)
            -- Parse and run type inference
            HsModule _ _ _ _ decls <- parse s
            mapM_ inferDecl decls
            getVariableTypes


testBindings :: String -> [(Id, QuantifiedType)] -> TestTree
testBindings s cases = testCase (deline s) $ do
    let (etypes, state) = inferModule s
    ts <- unpackEither etypes
    --traceM (unpack $ pShow ts)
    let addDebugInfo action = catchError action (\err -> throwError $ err ++ "\n" ++ unpack (pShow state))
        check (name, Quantified _ t) = either assertFailure return $ runExcept $ addDebugInfo $
            case M.lookup name ts of
                Nothing -> throwError "Variable not in environment"
                Just (Quantified _ t') -> do
                    sub <- mgu t t'
                    let (s1, s2) = (applySub sub t, applySub sub t')
                    unless (s1 == s2) (throwError $ printf "Substitutions not equal: %s vs %s" (show s1) (show s2))
    mapM_ check cases

testBindingsFail :: String -> TestTree
testBindingsFail s = testCase ("Fails: " ++ s') $ assertBool (s' ++ ": " ++ unpack (pShow types)) (isLeft types)
    where s' = deline s
          (types, _) = inferModule s

unpackEither :: Either String b -> IO b
unpackEither = either assertFailure return

test :: TestTree
test = let
        [a, b] = [ TypeVariable s KindStar | s <- ["a", "b"] ]
        [ta, tb] = map TypeVar [a, b]
        in
        testGroup "Typechecking"
    [
        -- Simple literal type checks
        let s = "x = 5"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)]
    ,
        let s = "x = 1.2"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Fractional" ta) ta)]
    ,
        let s = "x = 'a'"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeChar)]
    ,
        let s = "x = \"ab\""
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeString)]
    ,
        -- Data constructors
        let s = "x = True"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "x = False"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        testBindingsFail "x = Foo"
    ,
        -- Pattern matching
        let s = "(x, y) = (1, True)" 
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        let s = "(x, _, _) = (True, False, True)"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "a@(x, _, _) = (True, False, True)"
            t = makeTuple (replicate 3 typeBool)
        in testBindings s
            [ ("x", Quantified S.empty $ Qualified S.empty typeBool)
            , ("a", Quantified S.empty $ Qualified S.empty t) ]
    ,
        let s = "a@(_, y) = (1, True)"
            t = makeTuple [ta, typeBool]
        in testBindings s
            [ ("y", Quantified S.empty $ Qualified S.empty typeBool)
            , ("a", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) t) ]
    ,
        let s = "(x, y) = (1, (True))"
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        testBindingsFail "(x, y) = True" 
    ,
        let s = "(x, (y, z, w)) = (1, (True, False, \"Hi\"))" 
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool)
            , ("z", Quantified S.empty $ Qualified S.empty typeBool)
            , ("w", Quantified S.empty $ Qualified S.empty typeString) ]
        -- TODO(kc506): Test pattern matching with data constructors
    ,
        let s = "x = (+)"
            t = makeFun [ta, ta] ta
        in testBindings s [("+", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) t)]
    ,
        -- Function application (prefix and infix)
        let s = "x = (+) 3 4" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)]
    ,
        let s = "x = 1 + 2" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)]
    ,
        let s = "x = 1 + 2 + 3" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta)]
    ,
        let s = "x = 1 + 2\ny = x + 3"
            q = Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Num" ta) ta
        in testBindings s [("x", q), ("y", q)]
    ,
    -- TODO(kc506): This doesn't fail as `Num Bool` doesn't fail - check paper for where it should
    -- getQualifiedTypeFrom typechecker state? Revert from qualified types and make getting the qualified version an
    -- explicit operation?
        testBindingsFail "x = 1 && True"
    ,
        -- Lambdas
        let s = "x = \\y -> 1 + y"
            q = S.singleton $ IsInstance "Num" ta
        --in testBindings s [("y", Qualified q t), ("x", Qualified q (makeFun [t] t))]
        in testBindings s
            [ ("y", Quantified (S.singleton a) $ Qualified q ta)
            , ("x", Quantified (S.singleton a) $ Qualified q (makeFun [ta] ta)) ]
    ,
        let s = "x = (\\y -> False && y)"
        in testBindings s
            [ ("y", Quantified S.empty $ Qualified S.empty typeBool)
            , ("x", Quantified S.empty $ Qualified S.empty (makeFun [typeBool] typeBool))]
    ,
        let s = "x = (\\f -> f True)"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty (makeFun [makeFun [typeBool] ta] ta))]
    ,
        let s = "x = (\\f -> f True) (\\y -> not (not y))"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "y = let f = \\x -> x in f 5"
            tf = makeFun [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance "Num" tb) tb) ]
    ,
        let s = "y = let f = \\x -> x in f 5 + f 6"
            tf = makeFun [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance "Num" tb) tb) ]
    ,
        let s = "a = let f = \\x -> x\n" ++
                "        g = \\y z -> z\n" ++
                "    in g (f 5) (f True)"
        in testBindings s [("a", Quantified S.empty (Qualified S.empty typeBool))]
    ,
        -- Should fail because f is non-quantified as it's a parameter so can only be applied to one type.
        -- Contrast to the above where f is bound in a let-expression so is quantified
        let s = "let const = \\x y -> y in (\\f -> const (f 5) (f True)) (\\x -> x)"
        in testBindingsFail s
    ]