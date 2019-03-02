{-# LANGUAGE FlexibleContexts #-}

module Backend.ILAANFSpec where

import           BasicPrelude            hiding (head)
import           Control.Monad.Except    (MonadError, runExcept, runExceptT, throwError, liftEither)
import qualified Data.Map                as M
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           Language.Haskell.Parser (ParseResult(..), parseModule)
import           Language.Haskell.Syntax
import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCase)
import           TextShow                (showt)

import           AlphaEq                 (alphaEqError)
import           Names                   (convertName)
import           Preprocessor.Info       (getClassInfo)
import           Backend.Deoverload      (deoverloadModule, deoverloadQuantType, runDeoverload)
import qualified Backend.Deoverload      as Deoverload
import           Backend.ILA             hiding (Expr(..), makeError, makeList, makeTuple)
import           Backend.ILAANF
import           ExtraDefs
import           Logger                  (runLoggerT)
import           NameGenerator           (evalNameGenerator, freshDummyTypeVarName)
import           Typechecker.Hardcoded   (builtinClasses, builtinDictionaries, builtinKinds)
import           Typechecker.Typechecker
import           Typechecker.Types       hiding (makeFunUnsafe, makeList, makeTuple)
import qualified Typechecker.Types       as T

parse :: MonadError Text m => Text -> m HsModule
parse s = case parseModule $ unpack s of
    ParseOk m           -> return m
    ParseFailed loc msg -> throwError $ pack msg <> ": " <> showt loc

makeTest :: Text -> [Binding AnfRhs] -> TestTree
makeTest input expected = testCase (unpack $ deline input) $
    case evalNameGenerator (runLoggerT $ runExceptT foo) 0 of
        (Left err, logs) -> assertFailure $ unpack $ unlines [err, "Logs:", unlines logs]
        (Right binds, logs) -> case runExcept $ alphaEqError (S.fromList expected) (S.fromList binds) of
            Left err -> assertFailure $ unpack $ unlines [err, unlines $ map showt expected, "vs", unlines $ map showt binds, "Logs:", unlines logs]
            _ -> return ()
    where foo = do
            m <- parse input
            let moduleClassInfo = getClassInfo m
            (m', ts) <- evalTypeInferrer (inferModuleWithBuiltins m)
            (dResult, dState) <- lift $ runDeoverload (deoverloadModule moduleClassInfo m') ts builtinKinds builtinClasses
            m'' <- liftEither $ runExcept dResult
            -- Convert the overloaded types (Num a => a) into deoverloaded types (Num a -> a).
            dets <- mapM deoverloadQuantType ts
            -- Run the ILA conversion on the deoverloaded module+types
            let dictNames = S.map convertName $ M.keysSet moduleClassInfo
            evalConverter (toIla m'' >>= ilaToAnf) M.empty dets builtinKinds (Deoverload.dictionaries dState) dictNames


test :: TestTree
test = testGroup "ILA-ANF"
    [
    --    let input = "f = \\x -> x"
    --        fBody = Lam "x" a $ Complex $ Case (Trivial $ Var "x" a) ["x'"] [ Alt Default $ Trivial $ Var "x'" a]
    --        fType = T.makeFunUnsafe [a] a
    --        fWrappedType = T.makeTuple [T.makeFunUnsafe [a] a]
    --        fBodyWrappedBinding = Complex $ evalNameGenerator (makeTupleUnsafe [Var "fBody'" fType]) 100
    --        fBodyWrapped = Trivial $ Var "fBodyWrapped" fWrappedType
    --        fWrapped = Trivial $ Var "fWrapped" fWrappedType
    --        expected =
    --            [ Rec $ M.fromList -- The pattern declaration for f, returning a tuple containing f's value
    --                [ ("fWrapped", Complex $
    --                    Case (Let "f0" fType fBody $ Trivial $ Var "f0" fType) ["fBody'"]
    --                        [ Alt Default $ Let "fBodyWrapped" fWrappedType fBodyWrappedBinding fBodyWrapped ]) ]
    --              -- f's actual binding, extracting the lambda body from the tuple
    --            , NonRec "f" $ Complex $
    --                Case fWrapped []
    --                    [ Alt (DataCon "(,)" ["f'"]) $ Trivial $ Var "f'" fType, errAlt fType ] ]
    --    in makeTest input expected
    --,
    --    let input = "f = \\x -> x + x ; y = f 1 :: Int"
    --        num = TypeCon $ TypeConstant "Num" (KindFun KindStar KindStar)
    --        numa = TypeApp num a KindStar
    --        plus = Var "+" (T.makeFunUnsafe [numa, a, a] a)
    --        fBody = Lam "d" numa $
    --            Case (Trivial $ Var "d" numa) ["d'"]
    --                [ Alt Default $ Lam "x" a $
    --                    Case (Var "x" a) ["x'"]
    --                        [Alt Default $ App (App (App plus (Var "d'" numa)) (Var "x'" a)) (Var "x" a)] ]
    --        expected = []
    --    in makeTest input expected
    ]
    where
        a:b:c:_ = map (\t -> TypeVar $ TypeVariable t KindStar) $ evalNameGenerator (replicateM 10 freshDummyTypeVarName) 1
        true = Var "True" typeBool
        false = Var "False" typeBool
        trueCon = DataCon "True"
        falseCon = DataCon "False"
        tupleCon = DataCon "(,)"
        consCon = DataCon ":"
        nilCon = DataCon "[]"
        errAlt t = Alt Default (Trivial $ makeError t)
