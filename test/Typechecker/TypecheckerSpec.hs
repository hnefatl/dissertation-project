{-# Language FlexibleContexts #-}

module Typechecker.TypecheckerSpec where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Syntax
import Language.Haskell.Parser

import ExtraDefs
import Names
import NameGenerator
import Typechecker.Types
import Typechecker.Unifier
import Typechecker.Typechecker
import Typechecker.Substitution

import Data.Either
import Data.Text.Lazy (unpack)
import Text.Printf
import Text.Pretty.Simple
import Control.Monad.State.Strict (get)
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Map as M

parse :: MonadError String m => String -> m HsModule
parse s = case parseModule s of
    ParseOk m -> return m
    ParseFailed loc msg -> throwError (msg ++ ": " ++ show loc)

inferModule' :: String -> (Either String (M.Map VariableName QuantifiedType), InferrerState)
inferModule' s = (runExcept out, state)
    where (out, state) = evalNameGenerator (runTypeInferrer $ catchError infer handler) 0
          handler err = get >>= \st -> throwError $ unlines [err, unpack $ pShow st]
          infer = do
            m <- parse s
            inferModuleWithBuiltins m

testBindings :: String -> [(String, QuantifiedType)] -> TestTree
testBindings s cases = testCase (deline s) $ do
    let (etypes, state) = inferModule' s
    ts <- unpackEither etypes
    let check (name, Quantified q1 t1) = either (assertFailure . printf "%s: %s" (show name)) return $ runExcept $ addDebugInfo $
            case M.lookup (VariableName name) ts of
                Nothing -> throwError "Variable not in environment"
                Just (Quantified q2 t2) -> do
                    sub <- mgu t1 t2
                    let (t1', t2') = pairmap (applySub sub) (t1, t2)
                        (q1', q2') = pairmap (getSubstitutedTypeVariables sub . S.map getTvName) (q1, q2)
                    unless (q1' == q2') (throwError $ printf "Quantifiers not equal: %s vs %s" (show q1') (show q2'))
                    unless (t1' == t2') (throwError $ printf "Qualified types not equal: %s vs %s" (show t1') (show t2'))
        addDebugInfo action = catchError action (\err -> throwError $ err ++ "\n" ++ unpack (pShow state))
    mapM_ check cases

testBindingsFail :: String -> TestTree
testBindingsFail s = testCase ("Fails: " ++ s') $ assertBool errMsg (isLeft result)
    where s' = deline s
          (result, state) = inferModule' s
          errMsg = printf "%s: Got %s\n%s" s' (unpack $ pShow result) (unpack $ pShow state)

unpackEither :: Either String b -> IO b
unpackEither = either assertFailure return

test :: TestTree
test = let
        [a, b] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b"]
        [ta, tb] = map TypeVar [a, b]
        [num, fractional] = map TypeConstantName ["Num", "Fractional"]
    in
        testGroup "Typechecking"
    [
        -- Simple literal type checks
        let s = "x = 5"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = 1.2"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance fractional ta) ta)]
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
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)
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
            , ("a", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) t) ]
    ,
        let s = "(x, y) = (1, (True))"
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        testBindingsFail "(x, y) = True" 
    ,
        let s = "(x, (y, z, w)) = (1, (True, False, \"Hi\"))" 
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool)
            , ("z", Quantified S.empty $ Qualified S.empty typeBool)
            , ("w", Quantified S.empty $ Qualified S.empty typeString) ]
        -- TODO(kc506): Test pattern matching with data constructors
    ,
        let s = "x = [True, False]"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty (makeList typeBool))]
    ,
        let s = "x = [1, 2, 3]"
        in testBindings s
            [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) (makeList ta))]
    ,
        let s = "x = [True, 2]"
        in testBindingsFail s
    ,
        let s = "x = [1, 2.2]"
        in testBindings s
            [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance fractional ta) (makeList ta))]
    ,
        let s = "x = (+)"
            t = makeFun [ta, ta] ta
        in testBindings s [("+", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) t)]
    ,
        -- Function application (prefix and infix)
        let s = "x = (+) 3" 
            t = makeFun [ta] ta
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) t)]
    ,
        let s = "x = (+) 3 4" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = 1 + 2" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = 1 + 2 + 3" 
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = 1 + 2\ny = x + 3"
            q = Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta
        in testBindings s [("x", q), ("y", q)]
    ,
        testBindingsFail "x = 1 && True"
    ,
        -- Lambdas
        let s = "x = \\y -> 1 + y"
            q = S.singleton $ IsInstance num ta
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified q (makeFun [ta] ta))
            , ("y", Quantified S.empty $ Qualified q ta) ]
    ,
        let s = "x = (\\y -> False && y)"
        in testBindings s
            [ ("x", Quantified S.empty $ Qualified S.empty (makeFun [typeBool] typeBool))
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        let s = "x = (\\f -> f True)"
        in testBindings s
            [ ("x", Quantified (S.singleton b) $ Qualified S.empty (makeFun [makeFun [typeBool] tb] tb))
            , ("f", Quantified S.empty $ Qualified S.empty (makeFun [typeBool] ta))]
    ,
        let s = "x = (\\f -> f True) (\\y -> not (not y))"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "y = let f = \\x -> x in f 5"
            tf = makeFun [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) tb) ]
    ,
        let s = "y = let f = \\x -> x in f 5 + f 6"
            tf = makeFun [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) tb) ]
    ,
    -- Disabled until we have dependency analysis: g should be typechecked in a different group to f
    --    let s = "a = let f = \\x -> x\n" ++
    --            "        g = \\y z -> z\n" ++
    --            "    in g (f 5) (f True)"
    --    in testBindings s [(Id "a", Quantified S.empty (Qualified S.empty typeBool))]
    --,
        -- Should fail because f is non-quantified as it's a parameter so can only be applied to one type.
        -- Contrast to the above where f is bound in a let-expression so is quantified
        let s = "x = let const = \\x y -> y in (\\f -> const (f 5) (f True)) (\\x -> x)"
        in testBindingsFail s
    ,
        let s = "x = (\\(y, z) -> y + z) (1, 2)"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = (\\[y, z] -> y && z) [True, False]"
        in testBindings s
            [ ("x", Quantified S.empty $ Qualified S.empty typeBool)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool)
            , ("z", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        let s = "x = if True then 1 else 2"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta)]
    ,
        let s = "x = if True then 1.2 else 2"
        in testBindings s [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance fractional ta) ta)]
    ,
        let s = "x = if True then True else False"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "x = if 1 then True else False"
        in testBindingsFail s
    ,
        let s = "x = if False then 1 else True"
        in testBindingsFail s
    ,
        let s = "_ = let { even = (\\x -> if x == 0 then True else odd (x - 1)) ; odd = (\\y -> if y == 0 then False else even (x - 1)) } in even 10"
            helper t = makeFun [t] typeBool
        in testBindings s
            [ ("even", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) (helper ta))
            , ("odd", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) (helper tb)) ]
    ,
        let s = "_ = let const = \\a b -> a in let { f = \\x -> const (g True) (g x) ; g = \\y -> const (f True) (f y) } in True"
            t = makeFun [typeBool] ta
        in testBindings s
            [ ("const", Quantified (S.fromList [a,b]) $ Qualified S.empty $ makeFun [ta, tb] ta)
            , ("f", Quantified (S.singleton a) $ Qualified S.empty t)
            , ("g", Quantified (S.singleton a) $ Qualified S.empty t) ]
    ,
        let s = "_ = let { x = y ; y = x y } in True"
        in testBindingsFail s
    ,
        let s = "_ = let { id = \\x -> x ; g = \\y -> id (h y) ; h = \\z -> g z } in True"
        in testBindings s
            [ ("id", Quantified (S.singleton a) $ Qualified S.empty ta)
            , ("g", Quantified (S.singleton b) $ Qualified S.empty tb)
            , ("h", Quantified (S.singleton b) $ Qualified S.empty tb) ]
    ]