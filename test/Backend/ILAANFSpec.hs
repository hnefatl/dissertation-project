{-# LANGUAGE FlexibleContexts #-}

module Backend.ILAANFSpec where

import           BasicPrelude            hiding (head)
import           Control.Monad.Except    (MonadError, runExceptT, throwError)
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
import           Backend.ILA             hiding (Expr(..), makeList, makeTuple, makeError)
import           Backend.ILAANF
import           ExtraDefs
import           Logger                  (clearLogs, runLoggerT, runLogger)
import           NameGenerator           (evalNameGenerator, freshDummyTypeVarName)
import           Names
import           Typechecker.Hardcoded   (builtinKinds)
import           Typechecker.Typechecker
import           Typechecker.Types       hiding (makeFun, makeList, makeTuple)
import qualified Typechecker.Types       as T

parse :: MonadError Text m => Text -> m HsModule
parse s = case parseModule $ unpack s of
    ParseOk m           -> return m
    ParseFailed loc msg -> throwError $ pack msg <> ": " <> showt loc

makeTest :: Text -> [Binding AnfComplex] -> TestTree
makeTest input expected = testCase (unpack $ deline input) $
    case evalNameGenerator (runLoggerT $ runExceptT foo) 0 of
        (Left err, logs) -> assertFailure $ unpack $ unlines [err, "Logs:", unlines logs]
        (Right binds, _) -> case runLogger $ runExceptT $ alphaEqError (S.fromList expected) (S.fromList binds) of
            (Left err, logs) -> assertFailure $ unpack $ unlines [err, showt expected, "vs", showt binds, "Logs:", unlines logs]
            _ -> return ()
    where foo = do
            m <- parse input
            (m', ts) <- evalTypeInferrer (inferModuleWithBuiltins m)
            clearLogs
            m'' <- evalDeoverload (deoverloadModule m')
            -- Convert the overloaded types (Num a => a) into deoverloaded types (Num a -> a).
            let dets = map deoverloadQuantType ts
            -- Run the ILA conversion on the deoverloaded module+types
            clearLogs
            evalConverter (toIla m'' >>= ilaToAnf) dets builtinKinds


test :: TestTree
test = testGroup "ILA-ANF"
    [
        let fBody = Lam "x" a $ Case (Var "x" a) ["x'"] [ Alt Default [] $ Var "x'" a]
            fType = T.makeFun [a] a
            [fBodyWrappedBinding] = makeTupleUnsafe [(Var "fBody'" fType, "fBodyWrapped")]
            fBodyWrapped = Var "fBodyWrapped" $ T.makeTuple [fType]
            fWrapped = Var "fWrapped" $ T.makeTuple [fType]
        in makeTest "f = \\x -> x"
            [ fBodyWrappedBinding -- The tuple (\x -> x)
            , Rec $ M.fromList -- The pattern declaration for f, returning a tuple containing f's value
                [ ("fWrapped", Case fBody ["fBody'"] [ Alt Default [] fBodyWrapped ]) ]
              -- f's actual binding, extracting the lambda body from the tuple
            , NonRec "f" $ Case fWrapped [] [ Alt (DataCon "(,)") ["f'"] $ Var "f'" fType, errAlt fType ]
            ]
    ]
    where a:b:c:_ = map (\t -> TypeVar $ TypeVariable t KindStar) $ evalNameGenerator (replicateM 10 freshDummyTypeVarName) 1
          true = Var "True" typeBool
          false = Var "False" typeBool
          trueCon = DataCon "True"
          falseCon = DataCon "False"
          tupleCon = DataCon "(,)"
          consCon = DataCon ":"
          nilCon = DataCon "[]"
          errAlt t = Alt Default [] (makeError t)
          plus t = Var "+" (T.makeFun [t, t] t)
