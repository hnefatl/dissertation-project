{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

module Backend.ILB where

import           Backend.ILA                 (Alt(..), Binding(..), Literal(..))
import qualified Backend.ILAANF              as ANF
import           BasicPrelude
import           Control.Monad.Except        (Except, ExceptT, MonadError, liftEither, runExceptT, throwError)
import           Control.Monad.Extra         (ifM)
import           Control.Monad.Reader        (Reader, asks, runReader)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           ExtraDefs                   (secondM)
import           Names
import           Preprocessor.ContainedNames
import           TextShow                    (TextShow, showb, showt)

-- Datatypes inspired by STG:
-- https://github.com/ghc/ghc/blob/6353efc7694ba8ec86c091918e02595662169ae2/compiler/stgSyn/StgSyn.hs
-- In ILB as in STG, Case expressions are the only place where evaluations happen, and Let expressions are the only
-- place allocation happens.
data Arg = ArgLit Literal
         | ArgVar VariableName
    deriving (Eq, Ord)
data Exp = ExpLit Literal
         | ExpVar VariableName
         | ExpApp VariableName [Arg] -- Might need to add a type so we know what type of register to assign?
         | ExpConApp VariableName [Arg] -- Application of a constructor: require *all* the arguments to be present.
         | ExpCase Exp [VariableName] [Alt Exp] -- Scrutinee, variables the scrutinee's assigned to, alts
         | ExpLet VariableName Rhs Exp
    deriving (Eq, Ord)
data Rhs = RhsClosure [VariableName] Exp -- Thunks: if it has arguments, it's a function; otherwise it's a thunk.
    deriving (Eq, Ord)

instance TextShow Arg where
    showb (ArgLit l) = showb l
    showb (ArgVar v) = showb v
instance TextShow Exp where
    showb (ExpLit l)       = showb l
    showb (ExpVar v)       = showb v
    showb (ExpApp v as)    = intercalate " " $ showb v:map showb as
    showb (ExpConApp c as) = intercalate " " $ showb c:map showb as
    showb (ExpCase s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> intercalate " ; " (map showb as) <> " }"
    showb (ExpLet v r e) = "let " <> showb v <> " = " <> showb r <> " in " <> showb e
instance TextShow Rhs where
    showb (RhsClosure vs e) = "\\" <> showb vs <> " -> " <> showb e

data ConverterState = ConverterState
    { constructors :: S.Set VariableName }
    deriving (Eq, Ord, Show)
instance TextShow ConverterState where
    showb = fromString . show
newtype Converter a = Converter (ExceptT Text (Reader ConverterState) a)
    deriving (Functor, Applicative, Monad, MonadError Text)

runConverter :: Converter a -> S.Set VariableName -> Except Text a
runConverter (Converter inner) s = liftEither $ runReader (runExceptT inner) (ConverterState s)

isConstructor :: VariableName -> Converter Bool
isConstructor n = Converter $ asks (S.member n . constructors)

anfToIlb :: [Binding ANF.AnfRhs] -> Converter [Binding Rhs]
anfToIlb = mapM anfBindingToIlbBinding
anfBindingToIlbBinding :: Binding ANF.AnfRhs -> Converter (Binding Rhs)
anfBindingToIlbBinding (NonRec v r) = NonRec v <$> anfRhsToIlbRhs r
anfBindingToIlbBinding (Rec m)      = Rec . M.fromList <$> mapM (secondM anfRhsToIlbRhs) (M.toList m)

anfRhsToIlbRhs :: ANF.AnfRhs -> Converter Rhs
anfRhsToIlbRhs (ANF.Lam arg _ e) = do -- Aggregate any nested lambdas into one
    RhsClosure args body <- anfRhsToIlbRhs e -- TODO(kc506): If we add more constructors to RHS, update
    return $ RhsClosure (arg:args) body
anfRhsToIlbRhs (ANF.Complex e) = RhsClosure [] <$> anfComplexToIlbExp e

anfComplexToIlbExp :: ANF.AnfComplex -> Converter Exp
anfComplexToIlbExp (ANF.Let v _ r b)  = ExpLet v <$> anfRhsToIlbRhs r <*> anfComplexToIlbExp b
anfComplexToIlbExp (ANF.Case s bs as) = ExpCase <$> anfComplexToIlbExp s <*> pure bs <*> mapM anfAltToIlbAlt as
anfComplexToIlbExp (ANF.CompApp a)    = anfComplexToIlbApp a
anfComplexToIlbExp (ANF.Trivial t)    = maybe (throwError "Unexpected type in expression") return (anfTrivialToExp t)

anfComplexToIlbApp :: ANF.AnfApplication -> Converter Exp
anfComplexToIlbApp (ANF.TrivApp a) = case a of
    ANF.Var n _ -> ifM (isConstructor n) (return $ ExpConApp n []) (return $ ExpApp n [])
    e           -> throwError $ "Application to a " <> showt e
anfComplexToIlbApp (ANF.App a e) = (anfTrivialToArg e,) <$> anfComplexToIlbApp a >>= \case
    (Just arg, ExpConApp n args) -> return $ ExpConApp n (args ++ [arg])
    (Just arg, ExpApp n args) -> return $ ExpApp n (args ++ [arg])
    (Nothing, app) -> return app
    (_, app) -> throwError $ "Got non-application from " <> showt a <> ": " <> showt app

anfTrivialToArg :: ANF.AnfTrivial -> Maybe Arg
anfTrivialToArg (ANF.Var v _) = Just $ ArgVar v
anfTrivialToArg (ANF.Lit l _) = Just $ ArgLit l
anfTrivialToArg ANF.Type{}    = Nothing

anfTrivialToExp :: ANF.AnfTrivial -> Maybe Exp
anfTrivialToExp (ANF.Var v _) = Just $ ExpVar v
anfTrivialToExp (ANF.Lit l _) = Just $ ExpLit l
anfTrivialToExp ANF.Type{}    = Nothing

anfAltToIlbAlt :: Alt ANF.AnfComplex -> Converter (Alt Exp)
anfAltToIlbAlt (Alt c vs e) = Alt c vs <$> anfComplexToIlbExp e


instance HasFreeVariables Arg where
    getFreeVariables ArgLit{}   = return S.empty
    getFreeVariables (ArgVar v) = return $ S.singleton v
instance HasFreeVariables Exp where
    getFreeVariables ExpLit{} = return S.empty
    getFreeVariables (ExpVar v) = return $ S.singleton v
    getFreeVariables (ExpApp v as) = S.insert v <$> getFreeVariables as
    getFreeVariables (ExpConApp _ as) = getFreeVariables as
    getFreeVariables (ExpCase s vs cs) = S.difference <$> (S.union <$> getFreeVariables s <*> getFreeVariables cs) <*> pure (S.fromList vs)
    getFreeVariables (ExpLet v rhs e) = S.delete v <$> (S.union <$> getFreeVariables rhs <*> getFreeVariables e)
instance HasFreeVariables Rhs where
    getFreeVariables (RhsClosure vs e) = S.difference <$> getFreeVariables e <*> pure (S.fromList vs)