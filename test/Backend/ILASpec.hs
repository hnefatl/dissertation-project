{-# Language FlexibleContexts #-}

module Backend.ILASpec where

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
test = testGroup "Backend"
    [
        let t1:t2:t3:_ = map (VariableName . ("v" ++) . show) [5 :: Int ..]
            x = VariableName "x"
            mainBind = Rec $ M.fromList [(t1, Case (makeTuple [true, false]) [t2] [Alt Default [] $ makeTuple [Var t2]])]
            auxBind = NonRec x (Case (Var t1) [] [Alt (DataCon tupleCon) [t3] (Var t3), Alt Default [] makeError])
        in
            makeTest "x = (True, False)" [mainBind, auxBind]
        ,
        let t1:t2:t3:t4:t5:t6:t7:t8:t9:_ = map (VariableName . ("v" ++) . show) [8 :: Int ..]
            x = VariableName "x"
            y = VariableName "y"
            mainBind = Rec $ M.fromList [
                ( t1
                , Case (makeTuple [true, false]) []
                    [ Alt (DataCon tupleCon) [t4, t5] $
                        Case (Var t5) [t3] [Alt Default [] (Case (Var t4) [t2] [Alt Default [] $ makeTuple [Var t2, Var t3]])]
                    , Alt Default [] makeError] ) ]
            auxBinds =
                [ NonRec x (Case (Var t1) [] [Alt (DataCon tupleCon) [t6, t7] (Var t6), Alt Default [] makeError])
                , NonRec y (Case (Var t1) [] [Alt (DataCon tupleCon) [t8, t9] (Var t9), Alt Default [] makeError]) ]
        in
            makeTest "(x, y) = (True, False)" $ mainBind:auxBinds
    ]
    where true = Var $ VariableName "True"
          false = Var $ VariableName "False"
          tupleCon = VariableName "(,)"