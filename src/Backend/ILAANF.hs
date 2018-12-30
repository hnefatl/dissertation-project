{-# LANGUAGE FlexibleContexts #-}

module Backend.ILAANF where

import           Backend.ILA          (Alt(..), Binding(..), Literal(..))
import qualified Backend.ILA          as ILA
import           BasicPrelude
import           Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict      as M
import           NameGenerator        (MonadNameGenerator, freshVarName)
import           Names
import           TextShow             (TextShow, showb)
import           Data.Text            (unpack)
import           Typechecker.Types    (Type)
import qualified Typechecker.Types    as T

-- |The AST of trivial expressions in the administrative normal form of ILA
data AnfTrivial = Var VariableName Type
                | Lit Literal Type
                | Lam VariableName Type AnfComplex
                | Type Type
    deriving (Eq, Ord)
-- |The AST of complex expressions in the ANF form of ILA
data AnfComplex = App AnfTrivial AnfTrivial
                | Let VariableName Type AnfComplex AnfTrivial
                | Case AnfTrivial [VariableName] [Alt AnfTrivial]
                | Trivial AnfTrivial
    deriving (Eq, Ord)
instance TextShow AnfTrivial where
    showb (Var v t)   = showb v <> " :: " <> showb t
    showb (Lit l t)   = showb l <> " :: " <> showb t
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Type t)    = showb t
instance TextShow AnfComplex where
    showb (Trivial e) = showb e
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> cases <> " }"
        where cases = mconcat $ intersperse " ; " $ map showb as

-- Need function to get the free variables of an ILA-ANF expression, so we know what free variables a thunk has. Needed
-- for STG translation? Info tables? Can write as a new get_ContainedName function.

getAnfTrivialType :: MonadError Text m => AnfTrivial -> m Type
getAnfTrivialType (Var _ t)   = return t
getAnfTrivialType (Lit _ t)   = return t
getAnfTrivialType (Lam _ t e) = T.makeFun [t] <$> getAnfComplexType e
getAnfTrivialType (Type t)    = return t
getAnfComplexType :: MonadError Text m => AnfComplex -> m Type
getAnfComplexType (Trivial e) = getAnfTrivialType e
getAnfComplexType (App e1 e2) = do
    e2Type <- getAnfTrivialType e2
    (argType, retType) <- T.unwrapFun =<< getAnfTrivialType e1
    when (argType /= e2Type) $ throwError "Invalid arg type"
    return retType
getAnfComplexType (Let _ _ _ e) = getAnfTrivialType e
getAnfComplexType (Case _ _ []) = throwError $ "No alts in case"
getAnfComplexType (Case _ _ (Alt _ _ e:_)) = getAnfTrivialType e


makeTuple :: MonadError Text m => [(AnfTrivial, VariableName)] -> m [Binding AnfComplex]
makeTuple xl = do
    ts <- mapM (getAnfTrivialType . fst) xl
    let conType = T.makeFun ts (T.makeTuple ts)
        helper _ [] = return []
        helper l ((e, n):ps) = do
            (_, retType) <- T.unwrapFun =<< getAnfTrivialType l
            (NonRec n (App l e):) <$> helper (Var n retType) ps
    helper (Var "(,)" conType) xl
makeTupleUnsafe :: [(AnfTrivial, VariableName)] -> [Binding AnfComplex]
makeTupleUnsafe = either (error . unpack) id . makeTuple

makeError :: Type -> AnfTrivial
makeError t = Var "error" t


ilaToAnf :: (MonadNameGenerator m, MonadError Text m) => [Binding ILA.Expr] -> m [Binding AnfComplex]
ilaToAnf = fmap concat . mapM ilaBindingToAnf

ilaBindingToAnf :: (MonadNameGenerator m, MonadError Text m) => Binding ILA.Expr -> m [Binding AnfComplex]
ilaBindingToAnf (NonRec v e) = do
    (bs, e') <- ilaExpToAnf e
    return $ NonRec v e':bs
ilaBindingToAnf (Rec m) = do
    (bss, es') <- fmap unzip $ forM (M.toList m) $ \(v, e) -> do
        (bs, e') <- ilaExpToAnf e
        return (bs, (v, e'))
    return $ (Rec $ M.fromList es'):concat bss

ilaExpToAnf :: (MonadNameGenerator m, MonadError Text m) => ILA.Expr -> m ([Binding AnfComplex], AnfComplex)
ilaExpToAnf (ILA.Var v t) = return ([], Trivial $ Var v t)
ilaExpToAnf (ILA.Lit l t) = return ([], Trivial $ Lit l t)
ilaExpToAnf (ILA.Lam v t b) = do
    (bs, b') <- ilaExpToAnf b
    return (bs, Trivial $ Lam v t b')
ilaExpToAnf (ILA.Type t) = return ([], Trivial $ Type t)
ilaExpToAnf (ILA.App e1 e2) = do
    (e1bs, e1') <- ilaExpToAnf e1
    (e2bs, e2') <- ilaExpToAnf e2
    (e1tbs, e1t) <- makeBinding e1'
    (e2tbs, e2t) <- makeBinding e2'
    return (concat [e1bs, e1tbs, e2bs, e2tbs], App e1t e2t)
ilaExpToAnf (ILA.Let v t e b) = do
    (ebs, e') <- ilaExpToAnf e
    (bbs, b') <- ilaExpToAnf b
    (bbs', b'') <- makeBinding b'
    return (ebs <> bbs <> bbs', Let v t e' b'')
ilaExpToAnf (ILA.Case s vs as) = do
    (sbs, s') <- ilaExpToAnf s
    (stbs, st) <- makeBinding s'
    (asbs, ats) <- unzip <$> mapM ilaAltToAnf as
    return (concat [sbs, stbs, concat asbs], Case st vs ats)

ilaAltToAnf :: (MonadNameGenerator m, MonadError Text m) => Alt ILA.Expr -> m ([Binding AnfComplex], Alt AnfTrivial)
ilaAltToAnf (Alt c vs e) = do
    (bs, e') <- ilaExpToAnf e
    (ebs, et) <- makeBinding e'
    return (bs <> ebs, Alt c vs et)

makeBinding :: (MonadNameGenerator m, MonadError Text m) => AnfComplex -> m ([Binding AnfComplex], AnfTrivial)
makeBinding (Trivial t) = return ([], t)
makeBinding e = do
    v <- freshVarName
    t <- getAnfComplexType e
    return ([NonRec v e], Var v t)
