{-# LANGUAGE FlexibleContexts #-}

module Backend.ILASpec where

import           BasicPrelude            hiding (head)
import           Control.Monad.Except    (MonadError, runExcept, runExceptT, throwError)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           Language.Haskell.Parser (ParseResult(..), parseModule)
import           Language.Haskell.Syntax
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCase)
import           TextShow                (showt)

import           AlphaEq                 (alphaEqError)
import           Backend.Deoverload      (deoverloadModule, deoverloadQuantType, evalDeoverload)
import           Backend.ILA
import           ExtraDefs
import           Logger                  (runLoggerT)
import           NameGenerator           (evalNameGenerator, freshDummyVarName)
import           Names
import           Typechecker.Hardcoded   (builtinClasses, builtinDictionaries, builtinKinds)
import           Typechecker.Typechecker
import           Typechecker.Types       hiding (makeFun, makeList, makeTuple)
import qualified Typechecker.Types       as T

parse :: MonadError Text m => Text -> m HsModule
parse s = case parseModule $ unpack s of
    ParseOk m           -> return m
    ParseFailed loc msg -> throwError $ pack msg <> ": " <> showt loc

makeTest :: Text -> [Binding Expr] -> TestTree
makeTest input expected = testCase (unpack $ deline input) $
    case evalNameGenerator (runLoggerT $ runExceptT foo) 0 of
        (Left err, logs) -> assertFailure $ unpack $ unlines [err, "Logs:", unlines logs]
        (Right binds, logs1) -> case runExcept $ alphaEqError (S.fromList expected) (S.fromList binds) of
            Left err -> assertFailure $ unpack $ unlines [err, unlines $ map showt expected, "vs", unlines $ map showt binds, "Logs:", unlines logs1]
            _ -> return ()
    where foo = do
            m <- parse input
            (m', ts) <- evalTypeInferrer (inferModuleWithBuiltins m)
            m'' <- evalDeoverload (deoverloadModule m') builtinDictionaries ts builtinKinds builtinClasses
            -- Convert the overloaded types (Num a => a) into deoverloaded types (Num a -> a).
            let dets = map deoverloadQuantType ts
            -- Run the ILA conversion on the deoverloaded module+types
            evalConverter (toIla m'') M.empty dets builtinKinds


test :: TestTree
test = testGroup "ILA"
    [
--        let t = T.makeTuple [typeBool, typeBool]
--            t' = T.makeFun [typeBool, typeBool] t
--            t'' = T.makeFun [t] $ T.makeTuple [t]
--            mainBind = Rec $ M.fromList [
--                ( t2
--                , Case (makeTuple' [true, false] t') [t1] [ Alt Default $ makeTuple' [Var t1 t] t'' ] )]
--            auxBind = NonRec x (Case (Var t2 $ T.makeTuple [t]) [] [Alt (tupleCon [t3]) (Var t3 t), errAlt t])
--        in makeTest "x = (True, False)" [mainBind, auxBind]
--    ,
--        let t = T.makeTuple [typeBool, typeBool]
--            t' = T.makeFun [typeBool, typeBool] t
--            mainBind = Rec $ M.fromList [
--                ( t5
--                , Case (makeTuple' [true, false] t') []
--                    [ Alt (tupleCon [t3, t4]) $
--                        Case (Var t4 typeBool) [t2]
--                            [ Alt Default (Case (Var t3 typeBool) [t1]
--                                [ Alt Default $ makeTuple' [Var t1 typeBool, Var t2 typeBool] t' ]) ]
--                    , errAlt t ] ) ]
--            auxBinds =
--                [ NonRec x (Case (Var t5 t) [] [Alt (tupleCon [t6, t7]) (Var t6 typeBool), errAlt typeBool])
--                , NonRec y (Case (Var t5 t) [] [Alt (tupleCon [t8, t9]) (Var t9 typeBool), errAlt typeBool]) ]
--        in makeTest "(x, y) = (True, False)" $ mainBind:auxBinds
--    ,
--        let boolList = T.makeList typeBool
--            boolListTuple = T.makeTuple [boolList]
--            err = errAlt boolListTuple
--            mainBind = Rec $ M.fromList [
--                ( t6
--                , Case (makeList' [false] typeBool) [t1]
--                    [ Alt (consCon [t2, t3]) $ Case (Var t3 boolList) []
--                        [ Alt (consCon [t4, t5]) $ Case (Var t5 boolList) []
--                            [ Alt nilCon $ Case (Var t4 typeBool) []
--                                [ Alt Default $ Case (Var t2 typeBool) []
--                                    [ Alt trueCon $ makeTuple [(Var t1 boolList, boolList)] , err ]
--                                ]
--                            , err ]
--                        , err ]
--                    , err ]
--                ) ]
--            auxBind = NonRec x (Case (Var t6 boolListTuple) [] [Alt (tupleCon [t7]) (Var t7 boolList), errAlt boolList])
--        in makeTest "x@[True, _] = [False]" [mainBind, auxBind]
--    ,
--        let a = TypeVar $ TypeVariable (TypeVariableName "a") KindStar
--            b = TypeVar $ TypeVariable (TypeVariableName "b") KindStar
--            fType = T.makeFun [a, b] a
--            lambdaBody =
--                Lam t2 a $ Case (Var t2 a) [t3]
--                    [ Alt Default $ Lam t4 b $ Case (Var t4 b) [t5]
--                        [ Alt Default $ Var t3 a] ]
--            mainBind = Rec $ M.fromList
--                [ (t6 , Case lambdaBody [t1] [ Alt Default $ makeTuple [(Var t1 fType, fType)] ]) ]
--            auxBind = NonRec f (Case (Var t6 $ T.makeTuple [fType]) [] [Alt (tupleCon [t7]) (Var t7 fType), errAlt fType])
--        in makeTest "f = \\x y -> x" [mainBind, auxBind]
--    ,
--        let head = Case (Var x typeBool) [] [Alt trueCon true, Alt falseCon false]
--            mainBinds =
--                [ Rec $ M.fromList [ (t2, Case true [t1] [ Alt Default $ makeTuple [(Var t1 typeBool, typeBool)] ]) ]
--                , Rec $ M.fromList [ (t5, Case head [t4] [ Alt Default $ makeTuple [(Var t4 typeBool, typeBool)] ]) ] ]
--            auxBinds =
--                [ NonRec x $ Case (Var t2 $ T.makeTuple [typeBool]) []
--                    [Alt (tupleCon [t3]) (Var t3 typeBool), errAlt typeBool]
--                , NonRec y $ Case (Var t5 $ T.makeTuple [typeBool]) []
--                    [Alt (tupleCon [t6]) (Var t6 typeBool), errAlt typeBool] ]
--        in makeTest "x = True ; y = if x then True else False" (mainBinds <> auxBinds)
--    ,
--        let mainBind = Rec $ M.fromList
--                [ (t2, Case true [t1] [Alt Default $ makeTuple [(Var t1 typeBool, typeBool)]]) ]
--            auxBind = NonRec x $ Case (Var t2 $ T.makeTuple [typeBool]) []
--                [ Alt (tupleCon [t3]) (Var t3 typeBool), errAlt typeBool ]
--        in makeTest "((x)) = (((True)))" [mainBind, auxBind]
--    ,
--        let a = TypeVar $ TypeVariable "a" KindStar
--            num = TypeCon $ TypeConstant "Num" (KindFun KindStar KindStar)
--            numa = TypeApp num a KindStar
--            numInt = TypeApp num typeInt KindStar
--            plus = Var "+" $ T.makeFun [numa, a, a] a
--            fBody = Lam "x2" numa $ Case (Var "x2" numa) ["x3"] -- \dNuma ->
--                [ Alt Default $ Lam "x4" a $ Case (Var "x4" a) ["x5"] -- \x ->
--                    [ Alt Default $
--                        App (App (App plus $ Var "x3" numa) $ Var "x5" a) $ Var "x5" a ] -- (+) dNuma x x
--                ]
--            fType = T.makeFun [numa, a] a
--            fTupleType = T.makeTuple [fType]
--            fIntType = T.makeFun [numInt, typeInt] typeInt
--            yBody = App (App (Var f fIntType) (Var "dNumInt" numInt)) (Lit (LiteralInt 1) typeInt)
--            yType = typeInt
--            yTupleType = T.makeTuple [yType]
--            mainBinds =
--                [ Rec $ M.fromList [("x1", Case fBody ["x6"] [Alt Default $ makeTuple [(Var "x6" fType, fType)]])]
--                , Rec $ M.fromList [("x7", Case yBody ["x8"] [Alt Default $ makeTuple [(Var "x8" yType, yType)]])] ]
--            auxBinds =
--                [ NonRec f $ Case (Var "x1" fTupleType) [] [ Alt (tupleCon ["x9"]) (Var "x9" fType), errAlt fType ]
--                , NonRec y $ Case (Var "x7" yTupleType) [] [ Alt (tupleCon ["t10"]) (Var "t10" yType), errAlt yType ] ]
--        in makeTest "f = \\x -> x + x ; y = f 1 :: Int" (mainBinds <> auxBinds)
    ]
    where t1:t2:t3:t4:t5:t6:t7:t8:t9:_ = evalNameGenerator (replicateM 10 freshDummyVarName) 0
          x = VariableName "x"
          y = VariableName "y"
          f = VariableName "f"
          true = Con (VariableName "True") typeBool
          false = Con (VariableName "False") typeBool
          trueCon = DataCon (VariableName "True") []
          falseCon = DataCon (VariableName "False") []
          tupleCon = DataCon (VariableName "(,)")
          consCon = DataCon (VariableName ":")
          nilCon = DataCon (VariableName "[]") []
          errAlt t = Alt Default (makeError t)
