{-# Language FlexibleContexts #-}

module Backend.ILASpec where

import Prelude hiding (head)
import Test.Tasty
import Test.Tasty.HUnit
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Control.Monad.Except
import qualified Data.Map as M
import qualified Data.Set as S

import ExtraDefs
import Names
import NameGenerator
import Typechecker.Typechecker
import Backend.ILA

parse :: MonadError String m => String -> m HsModule
parse s = case parseModule s of
    ParseOk m -> return m
    ParseFailed loc msg -> throwError (msg ++ ": " ++ show loc)

makeTest :: String -> [Binding] -> TestTree
makeTest input expected = testCase (deline input) $
    case runExcept foo of
        Left err -> assertFailure err
        Right binds -> assertEqual "" (S.fromList expected) (S.fromList binds)
    where foo = do
            m <- parse input
            let (eTypes, counter) = runNameGenerator (evalTypeInferrer $ inferModuleWithBuiltins m) 0
            ts <- eTypes
            evalNameGenerator (runConverter (toIla m) ts) counter


test :: TestTree
test = testGroup "ILA"
    [
        let t1:t2:t3:_ = map (VariableName . ("v" ++) . show) [5 :: Int ..]
            mainBind = Rec $ M.fromList [
                ( t2
                , Case (makeTuple [true, false]) [t1] [ Alt Default [] $ makeTuple [Var t1] ] )]
            auxBind = NonRec x (Case (Var t2) [] [Alt tupleCon [t3] (Var t3), errAlt])
        in makeTest "x = (True, False)" [mainBind, auxBind]
        ,
        let t1:t2:t3:t4:t5:t6:t7:t8:t9:_ = map (VariableName . ("v" ++) . show) [8 :: Int ..]
            mainBind = Rec $ M.fromList [
                ( t5
                , Case (makeTuple [true, false]) []
                    [ Alt tupleCon [t3, t4] $
                        Case (Var t4) [t2]
                            [ Alt Default [] (Case (Var t3) [t1] [Alt Default [] $ makeTuple [Var t1, Var t2]]) ]
                    , errAlt] ) ]
            auxBinds =
                [ NonRec x (Case (Var t5) [] [Alt tupleCon [t6, t7] (Var t6), errAlt])
                , NonRec y (Case (Var t5) [] [Alt tupleCon [t8, t9] (Var t9), errAlt]) ]
        in makeTest "(x, y) = (True, False)" $ mainBind:auxBinds
        ,
        let t1:t2:t3:t4:t5:t6:t7:_ = map (VariableName . ("v" ++) . show) [11 :: Int ..]
            mainBind = Rec $ M.fromList [
                ( t6
                , Case (makeList [false]) [t1]
                    [ Alt consCon [t2, t3] $ Case (Var t3) []
                        [ Alt consCon [t4, t5] $ Case (Var t5) []
                            [ Alt nilCon [] $ Case (Var t4) []
                                [ Alt Default [] $ Case (Var t2) []
                                    [ Alt trueCon [] $ makeTuple [Var t1]
                                    , errAlt ]
                                ]
                            , errAlt ]
                        , errAlt ]
                    , errAlt]
                ) ]
            auxBind = NonRec x (Case (Var t6) [] [Alt tupleCon [t7] (Var t7), errAlt])
        in makeTest "x@[True, _] = [False]" [mainBind, auxBind]
        ,
        let t1:t2:t3:t4:t5:t6:t7:_ = map (VariableName . ("v" ++) . show) [7 :: Int ..]
            lambdaBody =
                Lam t2 $ Case (Var t2) [t3] [ Alt Default [] $ Lam t4 $ Case (Var t4) [t5] [ Alt Default [] $ Var t3] ]
            mainBind = Rec $ M.fromList [ (t6 , Case lambdaBody [t1] [ Alt Default [] $ makeTuple [Var t1] ]) ]
            auxBind = NonRec f (Case (Var t6) [] [Alt tupleCon [t7] (Var t7), errAlt])
        in makeTest "f = \\x y -> x" [mainBind, auxBind]
        ,
        let t1:t2:t3:t4:t5:t6:_ = map (VariableName . ("v" ++) . show) [9 :: Int ..]
            head = Case (Var x) [] [Alt trueCon [] true, Alt falseCon [] false]
            mainBinds =
                [ Rec $ M.fromList [ (t2, Case true [t1] [ Alt Default [] $ makeTuple [Var t1] ]) ]
                , Rec $ M.fromList [ (t5, Case head [t4] [ Alt Default [] $ makeTuple [Var t4] ]) ] ]
            auxBinds =
                [ NonRec x $ Case (Var t2) [] [Alt tupleCon [t3] (Var t3), errAlt]
                , NonRec y $ Case (Var t5) [] [Alt tupleCon [t6] (Var t6), errAlt] ]
        in makeTest "x = True\ny = if x then True else False" (mainBinds ++ auxBinds)
        ,
        let t1:t2:t3:_ = map (VariableName . ("v" ++) . show) [3 :: Int ..]
            mainBind = Rec $ M.fromList [ (t2, Case true [t1] [Alt Default [] $ makeTuple [Var t1]]) ]
            auxBind = NonRec x $ Case (Var t2) [] [Alt tupleCon [t3] (Var t3), errAlt]
        in makeTest "((x)) = (((True)))" [mainBind, auxBind]
    ]
    where x = VariableName "x"
          y = VariableName "y"
          f = VariableName "f"
          true = Var $ VariableName "True"
          false = Var $ VariableName "False"
          trueCon = DataCon $ VariableName "True"
          falseCon = DataCon $ VariableName "False"
          tupleCon = DataCon $ VariableName "(,)"
          consCon = DataCon $ VariableName ":"
          nilCon = DataCon $ VariableName "[]"
          errAlt = Alt Default [] makeError