{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Backend.ILAANF where

import           AlphaEq              (AlphaEq, alphaEq')
import           Backend.ILA          (Alt(..), Binding(..), Literal(..))
import qualified Backend.ILA          as ILA
import           BasicPrelude
import           Control.DeepSeq      (NFData)
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict      as M
import           ExtraDefs            (secondM)
import           GHC.Generics         (Generic)
import           Logger               (MonadLogger, writeLog)
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
    deriving (Eq, Ord, Generic)
instance NFData AnfTrivial
-- |Applications of terms: specifically split out to ensure we only ever apply trivial arguments
data AnfApplication = App AnfApplication AnfTrivial
                    | TrivApp AnfTrivial
    deriving (Eq, Ord, Generic)
instance NFData AnfApplication
-- |Complex ANF expressions are the bread-and-butter: let expressions, case expressions.
-- Like GHC's STG, in ILA-ANF Case expressions are the only points of *evaluation*, and Let expressions are points of
-- *allocation* (but not the only points of allocation).
data AnfComplex = Let VariableName Type AnfRhs AnfComplex
                | Case AnfComplex Type [VariableName] [Alt AnfComplex]
                | CompApp AnfApplication
                | Trivial AnfTrivial
    deriving (Eq, Ord, Generic)
instance NFData AnfComplex
data AnfRhs = Lam VariableName Type AnfRhs -- Lambdas are only allowed at top-level RHSs
            | Complex AnfComplex
    deriving (Eq, Ord, Generic)
instance NFData AnfRhs
instance TextShow AnfTrivial where
    showb (Var v t) = showb v <> " :: " <> showb t
    showb (Con c t) = showb c <> " ;; " <> showb t
    showb (Lit l t) = showb l <> " :: " <> showb t
    showb (Type t)  = showb t
instance TextShow AnfApplication where
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (TrivApp e) = showb e
instance TextShow AnfComplex where
    showb (Trivial e) = showb e
    showb (CompApp e) = showb e
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s t bs as) = "case " <> showb s <> " :: " <> showb t <> " of " <> showb bs <> " { " <> cases <> " }"
        where cases = mconcat $ intersperse " ; " $ map showb as
instance TextShow AnfRhs where
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Complex c) = showb c


getAnfTrivialType :: MonadError Text m => AnfTrivial -> m Type
getAnfTrivialType (Var _ t) = return t
getAnfTrivialType (Con _ t) = return t
getAnfTrivialType (Lit _ t) = return t
getAnfTrivialType (Type t)  = return t
getAnfAppType :: (MonadError Text m, MonadLogger m) => AnfApplication -> m Type
getAnfAppType (TrivApp e) = getAnfTrivialType e
getAnfAppType (App e1 e2) = do
    e2Type <- getAnfTrivialType e2
    (argType, retType) <- T.unwrapFun =<< getAnfAppType e1
    when (argType /= e2Type) $ throwError $ unlines ["Mismatched arg types:", showt argType, showt e2Type, showt e1, showt e2]
    return retType
getAnfComplexType :: (MonadError Text m, MonadLogger m) => AnfComplex -> m Type
getAnfComplexType (Trivial e)              = getAnfTrivialType e
getAnfComplexType (CompApp e)              = getAnfAppType e
getAnfComplexType (Let _ _ _ e)            = getAnfComplexType e
getAnfComplexType (Case _ _ _ [])          = throwError "No alts in case"
getAnfComplexType (Case _ _ _ (Alt _ e:_)) = getAnfComplexType e
getAnfRhsType :: (MonadError Text m, MonadLogger m) => AnfRhs -> m Type
getAnfRhsType (Lam _ t e) = T.makeFun [t] =<< getAnfRhsType e
getAnfRhsType (Complex c) = getAnfComplexType c


ilaToAnf :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => [Binding ILA.Expr] -> m [Binding AnfRhs]
ilaToAnf bs = writeLog "ILAANF" >> mapM ilaBindingToAnf bs

ilaBindingToAnf :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => Binding ILA.Expr -> m (Binding AnfRhs)
ilaBindingToAnf (NonRec v e) = NonRec v <$> ilaExpToRhs e
ilaBindingToAnf (Rec m)      = Rec . M.fromList <$> mapM (secondM ilaExpToRhs) (M.toList m)

ilaExpToTrivial :: MonadError Text m => ILA.Expr -> m AnfTrivial
ilaExpToTrivial (ILA.Var v t) = return $ Var v t
ilaExpToTrivial (ILA.Con v t) = return $ Con v t
ilaExpToTrivial (ILA.Lit l t) = return $ Lit l t
ilaExpToTrivial (ILA.Type t)  = return $ Type t
ilaExpToTrivial e             = throwError $ "Non-trivial ILA to be converted to an ILA-ANF trivial: " <> showt e

ilaExpIsTrivial :: ILA.Expr -> Bool
ilaExpIsTrivial ILA.Var{}  = True
ilaExpIsTrivial ILA.Con{}  = True
ilaExpIsTrivial ILA.Lit{}  = True
ilaExpIsTrivial ILA.Type{} = True
ilaExpIsTrivial _          = False

ilaExpToApp  :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => ILA.Expr -> m AnfComplex
ilaExpToApp e@ILA.Con{} = CompApp . TrivApp <$> ilaExpToTrivial e
ilaExpToApp e@ILA.App{} = do
    (fun, args) <- ILA.unmakeApplication e
    if ilaExpIsTrivial fun then do
        -- If we're looking at an application to a trivial value (eg. `f x`) then just translate it direct
        fun' <- TrivApp <$> ilaExpToTrivial fun
        makeBindings args (return . CompApp . foldl' App fun')
    else do
        -- If the application is complex like `(\x -> x) y` then let-bind the function to a variable and use that in the
        -- application
        fun' <- ilaExpToRhs fun
        v <- freshVarName
        t <- getAnfRhsType fun'
        Let v t fun' <$> makeBindings args (return . CompApp . foldl' App (TrivApp $ Var v t))
ilaExpToApp e = throwError $ "Non-application ILA to be converted to an ILA-ANF application: " <> showt e

ilaExpToComplex :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => ILA.Expr -> m AnfComplex
ilaExpToComplex e@ILA.Var{}        = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.Con{}        = ilaExpToApp e
ilaExpToComplex e@ILA.Lit{}        = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.Type{}       = Trivial <$> ilaExpToTrivial e
ilaExpToComplex e@ILA.App{}        = ilaExpToApp e
ilaExpToComplex e@ILA.Lam{}        = makeBinding e (return . Trivial) -- `\x -> x` into `let v = \x -> x in v`
ilaExpToComplex (ILA.Let v t e b)  = Let v t <$> ilaExpToRhs e <*> ilaExpToComplex b
ilaExpToComplex (ILA.Case s vs as) = Case <$> ilaExpToComplex s <*> ILA.getExprType s <*> pure vs <*> mapM ilaAltToAnf as

ilaExpToRhs :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => ILA.Expr -> m AnfRhs
ilaExpToRhs (ILA.Lam v t b) = Lam v t <$> ilaExpToRhs b
ilaExpToRhs e               = Complex <$> ilaExpToComplex e

ilaAltToAnf :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => Alt ILA.Expr -> m (Alt AnfComplex)
ilaAltToAnf (Alt c e) = Alt c <$> ilaExpToComplex e

makeBinding :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => ILA.Expr -> (AnfTrivial -> m AnfComplex) -> m AnfComplex
makeBinding e makeBody = ilaExpToRhs e >>= \case
    Complex (Trivial t) -> makeBody t
    r -> do
        n <- freshVarName
        t <- getAnfRhsType r
        Let n t r <$> makeBody (Var n t)

makeBindings :: (MonadNameGenerator m, MonadError Text m, MonadLogger m) => [ILA.Expr] -> ([AnfTrivial] -> m AnfComplex) -> m AnfComplex
makeBindings as makeBody = helper as []
    where helper [] ns     = makeBody ns
          helper (e:es) ns = makeBinding e (\n -> helper es (n:ns))

instance AlphaEq AnfTrivial where
    alphaEq' (Var n1 t1) (Var n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (Con n1 t1) (Con n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (Lit l1 t1) (Lit l2 t2) = alphaEq' l1 l2 >> alphaEq' t1 t2
    alphaEq' (Type t1) (Type t2)     = alphaEq' t1 t2
    alphaEq' e1 e2                   = throwError $ unlines [ "AnfTrivial mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq AnfApplication where
    alphaEq' (App e1a e1b) (App e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (TrivApp e1) (TrivApp e2) = alphaEq' e1 e2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfApplication mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq AnfComplex where
    alphaEq' (Let v1 t1 e1a e1b) (Let v2 t2 e2a e2b) = do
        alphaEq' v1 v2
        alphaEq' t1 t2
        alphaEq' e1a e2a
        alphaEq' e1b e2b
    alphaEq' (Case e1 t1 vs1 as1) (Case e2 t2 vs2 as2) = alphaEq' e1 e2 >> alphaEq' t1 t2 >> alphaEq' vs1 vs2 >> alphaEq' as1 as2
    alphaEq' (Trivial e1) (Trivial e2) = alphaEq' e1 e2
    alphaEq' (CompApp e1) (CompApp e2) = alphaEq' e1 e2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfComplex mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq AnfRhs where
    alphaEq' (Lam v1 t1 e1) (Lam v2 t2 e2) = alphaEq' v1 v2 >> alphaEq' t1 t2 >> alphaEq' e1 e2
    alphaEq' (Complex c1) (Complex c2)     = alphaEq' c1 c2
    alphaEq' e1 e2                         = throwError $ unlines [ "AnfRhs mismatch:", showt e1, "vs", showt e2 ]
