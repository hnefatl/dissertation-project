{-# LANGUAGE FlexibleContexts #-}

module Backend.ILAANF where

import           Backend.ILA          (Alt(..), Binding(..), Literal(..))
import qualified Backend.ILA          as ILA
import           BasicPrelude
import           Control.Monad.Except (MonadError, runExceptT, throwError)
import qualified Data.Map.Strict      as M
import           Data.Text            (unpack)
import           ExtraDefs            (secondM)
import           NameGenerator        (MonadNameGenerator, freshVarName)
import           Names
import           TextShow             (TextShow, showb, showt)
import           Typechecker.Types    (Type)
import qualified Typechecker.Types    as T

-- These datatypes are inspired by the grammar in https://github.com/ghc/ghc/blob/6353efc7694ba8ec86c091918e02595662169ae2/compiler/coreSyn/CorePrep.hs#L144-L160
-- |Trivial ANF expressions are "atoms": variables, literals, types.
data AnfTrivial = Var VariableName Type
                | Con VariableName Type
                | Lit Literal Type
                | Type Type
    deriving (Eq, Ord)
-- |Applications of terms: specifically split out to ensure we only ever apply trivial arguments
data AnfApplication = App AnfApplication AnfTrivial
                    | TrivApp AnfTrivial
    deriving (Eq, Ord)
-- |Complex ANF expressions are the bread-and-butter: let expressions, case expressions.
-- Like GHC's STG, in ILA-ANF Case expressions are the only points of *evaluation*, and Let expressions are points of
-- *allocation* (but not the only points of allocation).
data AnfComplex = Let VariableName Type AnfRhs AnfComplex
                | Case AnfComplex [VariableName] [Alt AnfComplex]
                | CompApp AnfApplication
                | Trivial AnfTrivial
    deriving (Eq, Ord)
data AnfRhs = Lam VariableName Type AnfRhs -- Lambdas are only allowed at top-level RHSs
            | Complex AnfComplex
    deriving (Eq, Ord)
instance TextShow AnfTrivial where
    showb (Var v t) = showb v <> " :: " <> showb t
    showb (Con v t) = showb v <> " :: " <> showb t
    showb (Lit l t) = showb l <> " :: " <> showb t
    showb (Type t)  = showb t
instance TextShow AnfApplication where
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (TrivApp e) = showb e
instance TextShow AnfComplex where
    showb (Trivial e) = showb e
    showb (CompApp e) = showb e
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> cases <> " }"
        where cases = mconcat $ intersperse " ; " $ map showb as
instance TextShow AnfRhs where
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Complex c) = showb c

-- Need function to get the free variables of an ILA-ANF expression, so we know what free variables a thunk has. Needed
-- for STG translation? Info tables? Can write as a new get_ContainedName function.

getAnfTrivialType :: MonadError Text m => AnfTrivial -> m Type
getAnfTrivialType (Var _ t) = return t
getAnfTrivialType (Con _ t) = return t
getAnfTrivialType (Lit _ t) = return t
getAnfTrivialType (Type t)  = return t
getAnfAppType :: MonadError Text m => AnfApplication -> m Type
getAnfAppType (TrivApp e) = getAnfTrivialType e
getAnfAppType (App e1 e2) = do
    e2Type <- getAnfTrivialType e2
    (argType, retType) <- T.unwrapFun =<< getAnfAppType e1
    when (argType /= e2Type) $ throwError "Invalid arg type"
    return retType
getAnfComplexType :: MonadError Text m => AnfComplex -> m Type
getAnfComplexType (Trivial e)              = getAnfTrivialType e
getAnfComplexType (CompApp e)              = getAnfAppType e
getAnfComplexType (Let _ _ _ e)            = getAnfComplexType e
getAnfComplexType (Case _ _ [])            = throwError "No alts in case"
getAnfComplexType (Case _ _ (Alt _ _ e:_)) = getAnfComplexType e
getAnfRhsType :: MonadError Text m => AnfRhs -> m Type
getAnfRhsType (Lam _ t e) = T.makeFun [t] <$> getAnfRhsType e
getAnfRhsType (Complex c) = getAnfComplexType c


makeTuple :: (MonadNameGenerator m, MonadError Text m) => [AnfTrivial] -> m AnfComplex
makeTuple es = do
    ts <- mapM getAnfTrivialType es
    let conType = T.makeFun ts (T.makeTuple ts)
        base = TrivApp $ Var "(,)" conType
    return $ CompApp $ foldl' App base es
makeTupleUnsafe :: MonadNameGenerator m => [AnfTrivial] -> m AnfComplex
makeTupleUnsafe es = either (error . unpack) id <$> runExceptT (makeTuple es)

makeError :: Type -> AnfTrivial
makeError = Var "compilerError"


ilaToAnf :: (MonadNameGenerator m, MonadError Text m) => [Binding ILA.Expr] -> m [Binding AnfRhs]
ilaToAnf = mapM ilaBindingToAnf

ilaBindingToAnf :: (MonadNameGenerator m, MonadError Text m) => Binding ILA.Expr -> m (Binding AnfRhs)
ilaBindingToAnf (NonRec v e) = NonRec v <$> ilaExpToRhs e
ilaBindingToAnf (Rec m)      = Rec . M.fromList <$> mapM (secondM ilaExpToRhs) (M.toList m)

ilaExpToTrivial :: MonadError Text m => ILA.Expr -> m AnfTrivial
ilaExpToTrivial (ILA.Var v t) = return $ Var v t
ilaExpToTrivial (ILA.Con v t) = return $ Con v t
ilaExpToTrivial (ILA.Lit l t) = return $ Lit l t
ilaExpToTrivial (ILA.Type t)  = return $ Type t
ilaExpToTrivial e             = throwError $ "Non-trivial ILA to be converted to an ILA-ANF trivial: " <> showt e

ilaExpToApp  :: (MonadNameGenerator m, MonadError Text m) => ILA.Expr -> m AnfComplex
ilaExpToApp e@ILA.App{} = do
    (fun, args) <- ILA.unmakeApplication e
    fun' <- TrivApp <$> ilaExpToTrivial fun
    makeBindings args (return . CompApp . foldl' App fun')
ilaExpToApp e = throwError $ "Non-application ILA to be converted to an ILA-ANF application: " <> showt e

ilaExpToComplex :: (MonadNameGenerator m, MonadError Text m) => ILA.Expr -> m AnfComplex
ilaExpToComplex e@ILA.Var{}        = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.Con{}        = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.Lit{}        = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.Type{}       = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.App{}        = ilaExpToApp e
ilaExpToComplex e@ILA.Lam{}        = makeBinding e (return . Trivial) -- `\x -> x` into `let v = \x -> x in v`
ilaExpToComplex (ILA.Let v t e b)  = Let v t <$> ilaExpToRhs e <*> ilaExpToComplex b
ilaExpToComplex (ILA.Case s vs as) = Case <$> ilaExpToComplex s <*> pure vs <*> mapM ilaAltToAnf as

ilaExpToRhs :: (MonadNameGenerator m, MonadError Text m) => ILA.Expr -> m AnfRhs
ilaExpToRhs (ILA.Lam v t b) = Lam v t <$> ilaExpToRhs b
ilaExpToRhs e               = Complex <$> ilaExpToComplex e

ilaAltToAnf :: (MonadNameGenerator m, MonadError Text m) => Alt ILA.Expr -> m (Alt AnfComplex)
ilaAltToAnf (Alt c vs e) = Alt c vs <$> ilaExpToComplex e


makeBinding :: (MonadNameGenerator m, MonadError Text m) => ILA.Expr -> (AnfTrivial -> m AnfComplex) -> m AnfComplex
makeBinding e makeBody = do
    n <- freshVarName
    t <- ILA.getExprType e
    Let n t <$> ilaExpToRhs e <*> makeBody (Var n t)
makeBindings :: (MonadNameGenerator m, MonadError Text m) => [ILA.Expr] -> ([AnfTrivial] -> m AnfComplex) -> m AnfComplex
makeBindings as makeBody = helper as []
    where helper [] ns     = makeBody ns
          helper (e:es) ns = makeBinding e (\n -> helper es (n:ns))
