{-# LANGUAGE FlexibleContexts #-}

module Typechecker.TypecheckerSpec where

import           BasicPrelude
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.HUnit           (assertBool, assertEqual, assertFailure, testCase)

import           Language.Haskell.Parser    (ParseResult(..), parseModule)
import           Language.Haskell.Syntax

import           AlphaEq                    (alphaEq)
import           ExtraDefs                  (deline, pretty)
import           Logger                     (runLoggerT)
import           NameGenerator              (evalNameGenerator)
import           Names                      (TypeVariableName(..), VariableName(..))
import           Typechecker.Typechecker
import           Typechecker.Types

import           Control.Monad.Except       (MonadError, catchError, runExcept, throwError)
import           Control.Monad.State.Strict (get)
import           Data.Either                (isLeft)
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import           Data.Text                  (pack, unpack)
import           TextShow                   (showt)

parse :: MonadError Text m => Text -> m HsModule
parse s = case parseModule (unpack s) of
    ParseOk m           -> return m
    ParseFailed loc msg -> throwError $ pack msg <> ": " <> showt loc

inferModule' :: Text -> (Either Text (M.Map VariableName QuantifiedType), InferrerState, [Text])
inferModule' s = (runExcept out, state, logs)
    where ((out, state), logs) = evalNameGenerator (runLoggerT $ runTypeInferrer $ catchError infer handler) 0
          handler err = get >>= \st -> throwError $ unlines [err, pretty st, "Logs:", unlines logs]
          infer = do
            m <- parse s
            _ <- inferModuleWithBuiltins m -- Discard the updated tree and bound types
            getAllVariableTypes -- Explicitly pull all the variable types, not just the bound ones

testBindings :: Text -> [(VariableName, QuantifiedType)] -> TestTree
testBindings s cases = testCase (unpack $ deline s) $ do
    let (etypes, state, logs) = inferModule' s
    ts <- unpackEither etypes
    let getResult (name, qt1) = runExcept $ addDebugInfo $ case M.lookup name ts of
            Nothing  -> throwError "Variable not in environment"
            Just qt2 -> unless (alphaEq qt1 qt2) $ throwError $ unlines ["Got", showt qt2, "expected", showt qt1, unlines logs]
        addDebugInfo action = catchError action (\err -> throwError $ unlines [err, pretty state])
        check x@(name, _) = either (\e -> assertFailure $ unpack $ showt name <> ": " <> e) return (getResult x)
    mapM_ check cases

testBindingsFail :: Text -> TestTree
testBindingsFail s = testCase ("Fails: " <> unpack s') $ assertBool errMsg (isLeft result)
    where s' = deline s
          (result, state, logs) = inferModule' s
          errMsg = unpack $ unlines [s', "Got:", pretty result, "vs", pretty state, unlines logs]

unpackEither :: Either Text b -> IO b
unpackEither = either (assertFailure . unpack) return

test :: TestTree
test = let
        [a, b, c] = map (\n -> TypeVariable (TypeVariableName n) KindStar) ["a", "b", "c"]
        [ta, tb, tc] = map TypeVar [a, b, c]
        [num, fractional] = map TypeVariableName ["Num", "Fractional"]
        makeMaybe = applyTypeFunUnsafe (TypeCon $ TypeConstant (TypeVariableName "Maybe") (KindFun KindStar KindStar))
    in
        testGroup "Typechecking"
    [
        -- Utility checks
        let args = [makeMaybe ta, typeBool, typeString, tb]
            output = unmakeFun (makeFunUnsafe args tc)
        in testCase "unmakeFun . makeFunUnsafe" $ assertBool "" $ Right (args, tc) == output
    ,
        let [sa, sb, sc] = map (HsTyVar . HsIdent) ["a", "b", "c"]
            args = [sa, HsTyApp (HsTyCon $ UnQual $ HsIdent "Maybe") sb]
            output = unmakeSynFun (makeSynFun args sc)
        in testCase "unmakeSynFun . makeSynFun" $ assertEqual "" (args, sc) output
    ,
        let either = TypeConstant "Either" (KindFun KindStar $ KindFun KindStar KindStar)
            t = TypeApp (TypeApp (TypeCon either) ta (KindFun KindStar KindStar)) tb KindStar
            output = unmakeApp t
            expected = (TypeCon either, [ta, tb])
        in testCase "unmakeApp" $ assertEqual "" expected output
    ,
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
            t = makeTupleUnsafe (replicate 3 typeBool)
        in testBindings s
            [ ("x", Quantified S.empty $ Qualified S.empty typeBool)
            , ("a", Quantified S.empty $ Qualified S.empty t) ]
    ,
        let s = "a@(_, y) = (1, True)"
            t = makeTupleUnsafe [ta, typeBool]
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
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty (makeListUnsafe typeBool))]
    ,
        let s = "x = [1, 2, 3]"
        in testBindings s
            [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) (makeListUnsafe ta))]
    ,
        let s = "x = [True, 2]"
        in testBindingsFail s
    ,
        let s = "x = [1, 2.2]"
        in testBindings s
            [("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance fractional ta) (makeListUnsafe ta))]
    ,
        let s = "x = (+)"
            t = makeFunUnsafe [ta, ta] ta
        in testBindings s [("+", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) t)]
    ,
        -- Function application (prefix and infix)
        let s = "x = (+) 3"
            t = makeFunUnsafe [ta] ta
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
        let s = "x = 1 + 2 ; y = x + 3"
            q = Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta
        in testBindings s [("x", q), ("y", q)]
    ,
        testBindingsFail "x = 1 && True"
    ,
        -- Lambdas
        let s = "x = \\y -> 1 + y"
            q = S.singleton $ IsInstance num ta
        in testBindings s
            [ ("x", Quantified (S.singleton a) $ Qualified q (makeFunUnsafe [ta] ta))
            , ("y", Quantified S.empty $ Qualified q ta) ]
    ,
        let s = "x = (\\y -> False && y)"
        in testBindings s
            [ ("x", Quantified S.empty $ Qualified S.empty (makeFunUnsafe [typeBool] typeBool))
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        let s = "x = (\\f -> f True)"
        in testBindings s
            [ ("x", Quantified (S.singleton b) $ Qualified S.empty (makeFunUnsafe [makeFunUnsafe [typeBool] tb] tb))
            , ("f", Quantified S.empty $ Qualified S.empty (makeFunUnsafe [typeBool] ta))]
    ,
        let s = "x = (\\f -> f True) (\\y -> y)"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "x = (\\f -> f True) (\\y -> not (not y))"
        in testBindings s [("x", Quantified S.empty $ Qualified S.empty typeBool)]
    ,
        let s = "y = let f = \\x -> x in f 5"
            tf = makeFunUnsafe [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) tb) ]
    ,
        let s = "y = let f = \\x -> x in f 5 + f 6"
            tf = makeFunUnsafe [ta] ta
        in testBindings s
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty tf)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) tb) ]
    ,
        let s = "a = let f = \\x -> x ; " ++
                "        g = \\y z -> z ; " ++
                "    in g (f 5) (f True)"
        in testBindings s [("a", Quantified S.empty (Qualified S.empty typeBool))]
    ,
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
            helper t = makeFunUnsafe [t] typeBool
        in testBindings s
            [ ("even", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) (helper ta))
            , ("odd", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) (helper tb)) ]
    ,
        let s = "_ = let const = \\a b -> a in let { f = \\x -> const (g True) (g x) ; g = \\y -> const (f True) (f y) } in True"
            t = makeFunUnsafe [typeBool] ta
        in testBindings s
            [ ("const", Quantified (S.fromList [a,b]) $ Qualified S.empty $ makeFunUnsafe [ta, tb] ta)
            , ("f", Quantified (S.singleton a) $ Qualified S.empty t)
            , ("g", Quantified (S.singleton a) $ Qualified S.empty t) ]
    ,
        let s = "_ = let { x = y ; y = x y } in True"
        in testBindingsFail s
    ,
        let s = "_ = let { id = \\x -> x ; g = \\y -> id (h y) ; h = \\z -> g z } in True"
        in testBindings s
            [ ("id", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [ta] ta)
            , ("g", Quantified (S.fromList [b, c]) $ Qualified S.empty $ makeFunUnsafe [tb] tc)
            , ("h", Quantified (S.fromList [b, c]) $ Qualified S.empty $ makeFunUnsafe [tb] tc) ]
    ,
        testBindings "x = Nothing" [ ("x", Quantified (S.singleton a) $ Qualified S.empty $ makeMaybe ta) ]
    ,
        testBindings "x = Just True" [ ("x", Quantified S.empty $ Qualified S.empty $ makeMaybe typeBool) ]
    ,
        testBindings "f = \\(Just x) -> x"
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [makeMaybe ta] ta) ]
    ,
        testBindings "f = \\(Just x) -> x ; y = f (Just 5)"
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [makeMaybe ta] ta)
            , ("y", Quantified (S.singleton b) $ Qualified (S.singleton $ IsInstance num tb) tb) ]
    ,
        testBindings "f = \\(Just True) -> False ; y = f (Just False)"
            [ ("f", Quantified S.empty $ Qualified S.empty $ makeFunUnsafe [makeMaybe typeBool] typeBool)
            , ("y", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        testBindingsFail "f = \\Just -> True"
    ,
        testBindings "const = \\x y -> x ; z = const 1 2 ; w = const True False"
            [ ("const", Quantified (S.fromList [a, b]) $ Qualified S.empty $ makeFunUnsafe [ta, tb] ta)
            , ("z", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) $ ta)
            , ("w", Quantified S.empty $ Qualified S.empty typeBool) ]
    ,
        testBindings "const = \\x y -> x ; z = const True 1 ; w = const 1 2"
            [ ("const", Quantified (S.fromList [a, b]) $ Qualified S.empty $ makeFunUnsafe [ta, tb] ta)
            , ("z", Quantified S.empty $ Qualified S.empty typeBool)
            , ("w", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta) ]
    --,
    --    -- Test for ambiguity check support
    --    testBindingsFail "const = \\x y -> x ; z = const True (1 + 2)"
    ,
        testBindings "x = 0 :: Int" [("x", Quantified S.empty $ Qualified S.empty typeInt)]
    ,
        testBindings "f = \\x -> x + x ; y = (f :: Int -> Int) 0"
            [ ("f", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) $ makeFunUnsafe [ta] ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeInt) ]
    ,
        testBindings "f = \\x -> x ; y = f (0 :: Int)"
            [ ("f", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [ta] ta)
            , ("y", Quantified S.empty $ Qualified S.empty typeInt) ]
    ,
        testBindingsFail "f = (\\x -> x + x) :: Bool -> Bool"
    ,
        testBindingsFail "f = (\\x -> x + x) :: a -> a"
    ,
        testBindingsFail "f = (\\x -> x + x) :: Int -> a"
    ,
        testBindingsFail "f = (\\x -> (x :: Int) + x) :: a -> a"
    ,
        let t = makeFunUnsafe [typeInt, ta] ta
        in testBindings "class Foo b where { bar :: Int -> b -> b }"
            [ ("bar", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Foo" ta) t) ]
    ,
        testBindings "class Foo a where { bar :: a -> a } ; x = bar 5"
            [ ("bar", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Foo" ta) $ makeFunUnsafe [ta] ta)
            , ("x", Quantified (S.singleton b) $ Qualified (S.fromList [IsInstance "Foo" tb, IsInstance "Num" tb]) tb) ]
    ,
        testBindings "f x = x" [ ("f", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [ta] ta) ]
    ,
        testBindings "f 0 0 = 1 ; f x y = x + y"
            [ ("f", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) $ makeFunUnsafe [ta, ta] ta) ]
    ,
        testBindings "x = case True of { True -> 0 ; False -> 1 }"
            [ ("x", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance num ta) ta) ]
    ,
        testBindingsFail "x = case True of { True -> 0 ; 1 -> 1 }"
    ,
        testBindingsFail "x = case True of { True -> 0 ; False -> False }"
    ,
        testBindingsFail "x = case 0 of { True -> 0 ; False -> False }"
    ]
