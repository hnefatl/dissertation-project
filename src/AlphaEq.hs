{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module AlphaEq where

import           BasicPrelude
import           Control.Monad.Except       (ExceptT, Except, MonadError, catchError, liftEither, runExceptT, runExcept, throwError)
import           Control.Monad.Extra        (findM)
import           Control.Monad.State.Strict (MonadState, State, evalStateT, gets, modify, runState, get, put)
import           Data.Either                (isRight)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Language.Haskell.Syntax
import           TextShow                   (TextShow, showt)
import           TextShow.Instances         ()

import qualified Backend.ILA                as ILA
import qualified Backend.ILAANF             as ILAANF
import           ExtraDefs                  (synPrint)
import           Names
import           TextShowHsSrc              ()
import           Typechecker.Types

newtype AlphaEqM a = AlphaEqM { inner :: ExceptT Text (State (M.Map Text Text)) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map Text Text), MonadError Text)

class TextShow a => AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM ()
alphaEqBool :: AlphaEqM a -> AlphaEqM Bool
alphaEqBool x = catchError (x >> return True) (const $ return False)
alphaEqBool' :: AlphaEq a => a -> a -> AlphaEqM Bool
alphaEqBool' x = alphaEqBool . alphaEq' x

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = isRight $ runExcept $ alphaEqError x y
alphaEqError :: AlphaEq a => a -> a -> Except Text ()
alphaEqError x y = do
    z <- lift $ evalStateT (runExceptT $ inner $ alphaEq' x y) M.empty
    liftEither z

runAlphaEq :: AlphaEq a => a -> a -> (Maybe Text, M.Map Text Text)
runAlphaEq x y = (either Just (const Nothing) result, s)
    where (result, s) = runState (runExceptT $ inner $ alphaEq' x y) M.empty

-- |If the given computation fails, restore internal state to how it was before running the computation. If the
-- computation succeeds, leave the state alone.
checkpoint :: AlphaEqM a -> AlphaEqM a
checkpoint x = do
    s <- get
    catchError x (\err -> put s >> throwError err)

-- The workhorse: drives renaming names
instance AlphaEq Text where
    alphaEq' s1 s2
        | s1 == s2 = return ()
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) ->
                unless (x == s2 && y == s1) $ throwError $ unwords [s1, "and", s2, "already renamed to", x, "and", y]
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (Nothing, Just y) -> throwError $ s2 <> " has been renamed to " <> y <> " but " <> s1 <> " hasn't"
            (Just x, Nothing) -> throwError $ s1 <> " has been renamed to " <> x <> " but " <> s2 <> " hasn't"


-- Standard useful instances
-- Lists compare pairwise
instance AlphaEq a => AlphaEq [a] where
    alphaEq' [] []         = return ()
    alphaEq' (x:xs) (y:ys) = alphaEq' x y >> alphaEq' xs ys
    alphaEq' xs ys         = throwError $ unlines ["List length mismatch:", showt xs, "vs", showt ys]
-- Pairs compare... pairwise
instance (AlphaEq a, AlphaEq b) => AlphaEq (a, b) where
    alphaEq' (x1, y1) (x2, y2) = alphaEq' x1 x2 >> alphaEq' y1 y2
-- Sets compare in any order
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return () -- Both empty, both alpha equivalent
        | S.null s1 = throwError $ "Set is non-empty: " <> showt s2
        | S.null s2 = throwError $ "Set is non-empty: " <> showt s1
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            -- Find an alpha-eq element from the other set
            -- TODO(kc506): Need to rewrite this to use backtracking: taking the first alpha-eq match isn't sufficient,
            -- consider [x,y,(x,y)] and [a,b,(b,a)]. Should just be able to merge the `alphaEq' x` call with the
            -- follow-up into one action, then iterate that action? Remember to checkpoint appropriately.
            findM (alphaEqBool . checkpoint . alphaEq' x) (S.toList s2) >>= \case
                Nothing -> throwError $ unlines ["Failed to alpha-equate the sets", showt s1, "and", showt s2]
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse
-- Maps compare in any order by converting to sets
instance (Ord a, Ord b, AlphaEq a, AlphaEq b) => AlphaEq (M.Map a b) where
    alphaEq' m1 m2 = alphaEq' (S.fromList $ M.toList m1) (S.fromList $ M.toList m2)


-- Typechecker instances
instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2
instance AlphaEq VariableName where
    alphaEq' (VariableName s1) (VariableName s2) = alphaEq' s1 s2
instance AlphaEq TypeVariable where
    alphaEq' (TypeVariable n1 k1) (TypeVariable n2 k2) = do
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt n1 <> " vs " <> showt n2
        alphaEq' n1 n2
instance AlphaEq TypeConstant where
    alphaEq' (TypeConstant n1 k1) (TypeConstant n2 k2) = do
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt n1 <> " vs " <> showt n2
        alphaEq' n1 n2
instance AlphaEq Type where
    alphaEq' (TypeVar t1) (TypeVar t2) = alphaEq' t1 t2
    alphaEq' (TypeCon c1) (TypeCon c2) = alphaEq' c1 c2
    alphaEq' (TypeApp t1a t1b k1) (TypeApp t2a t2b k2) = do
        unless (k1 == k2) $ throwError $ "Kind mismatch: " <> showt k1 <> " vs " <> showt k2
        alphaEq' t1a t2a
        alphaEq' t1b t2b
    alphaEq' t1 t2 = throwError $ "Different types: " <> showt t1 <> " vs " <> showt t2
instance AlphaEq TypePredicate where
    alphaEq' (IsInstance c1 t1) (IsInstance c2 t2) = do
        unless (c1 == c2) $ throwError $ "Class mismatch: " <> showt c1 <> " vs " <> showt t1
        alphaEq' t1 t2
instance AlphaEq a => AlphaEq (Qualified a) where
    alphaEq' (Qualified quals1 t1) (Qualified quals2 t2) = alphaEq' t1 t2 >> alphaEq' quals1 quals2
instance AlphaEq QuantifiedType where
    alphaEq' (Quantified quants1 t1) (Quantified quants2 t2) = alphaEq' t1 t2 >> alphaEq' quants1 quants2


-- haskell-src instances
instance AlphaEq HsName where
    alphaEq' n1 n2 = alphaEq' (convertName n1 :: Text) (convertName n2)
instance AlphaEq HsQName where
    alphaEq' n1 n2 = alphaEq' (convertName n1 :: Text) (convertName n2)
instance AlphaEq HsOp where
    alphaEq' (HsVarOp n1) (HsVarOp n2) = alphaEq' n1 n2
    alphaEq' (HsConOp n1) (HsConOp n2) = unless (n1 == n2) $ throwError $ "HsConOp mismatch" <> showt n1 <> showt n2
    alphaEq' op1 op2 = throwError $ "HsOp mismatch: " <> showt op1 <> showt op2
instance AlphaEq HsQOp where
    alphaEq' (HsQVarOp n1) (HsQVarOp n2) = alphaEq' n1 n2
    alphaEq' (HsQConOp n1) (HsQConOp n2) = unless (n1 == n2) $ throwError $ "HsQConOp mismatch" <> showt n1 <> showt n2
    alphaEq' op1 op2 = throwError $ "HsQOp mismatch: " <> showt op1 <> showt op2
instance AlphaEq HsModule where
    alphaEq' (HsModule _ _ _ _ ds1) (HsModule _ _ _ _ ds2) = alphaEq' ds1 ds2
instance AlphaEq HsDecl where
    alphaEq' (HsPatBind _ pat1 rhs1 ds1) (HsPatBind _ pat2 rhs2 ds2) = do
        alphaEq' pat1 pat2
        alphaEq' rhs1 rhs2
        alphaEq' ds1 ds2
    alphaEq' (HsFunBind matches1) (HsFunBind matches2) = alphaEq' matches1 matches2
    alphaEq' (HsDataDecl _ _ name1 args1 cons1 derivings1) (HsDataDecl _ _ name2 args2 cons2 derivings2) = do
        alphaEq' name1 name2
        alphaEq' args1 args2
        alphaEq' cons1 cons2
        alphaEq' derivings1 derivings2
    alphaEq' (HsTypeSig _ names1 t1) (HsTypeSig _ names2 t2) = alphaEq' names1 names2 >> alphaEq' t1 t2
    alphaEq' (HsClassDecl _ ctx1 name1 args1 ds1) (HsClassDecl _ ctx2 name2 args2 ds2) = do
        alphaEq' ctx1 ctx2
        alphaEq' name1 name2
        alphaEq' args1 args2
        alphaEq' ds1 ds2
    alphaEq' d1 d2 = throwError $ unlines [ "Different declaration types:", showt d1, "vs", showt d2 ]
instance AlphaEq HsConDecl where
    alphaEq' (HsConDecl _ name1 ts1) (HsConDecl _ name2 ts2) = alphaEq' name1 name2 >> alphaEq' ts1 ts2
    alphaEq' HsRecDecl{} HsRecDecl{} = throwError "Record data constructors not supported"
    alphaEq' d1 d2 = throwError $ unlines ["Data constructor mismatch:", showt d1, "vs", showt d2]
instance AlphaEq HsMatch where
    alphaEq' (HsMatch _ name1 pats1 rhs1 wheres1) (HsMatch _ name2 pats2 rhs2 wheres2) = do
        alphaEq' name1 name2
        alphaEq' pats1 pats2
        alphaEq' rhs1 rhs2
        alphaEq' wheres1 wheres2
instance AlphaEq HsPat where
    alphaEq' (HsPVar n1) (HsPVar n2) = alphaEq' (convertName n1 :: Text) (convertName n2)
    alphaEq' (HsPLit l1) (HsPLit l2) =
        unless (l1 == l2) $ throwError $ unlines [ "Literal pat mismatch:", synPrint l1, "vs", synPrint l2 ]
    alphaEq' (HsPApp con1 ps1) (HsPApp con2 ps2) = do
        alphaEq' (convertName con1 :: Text) (convertName con2)
        alphaEq' ps1 ps2
    alphaEq' (HsPTuple ps1) (HsPTuple ps2) = alphaEq' ps1 ps2
    alphaEq' (HsPList ps1) (HsPList ps2) = alphaEq' ps1 ps2
    alphaEq' (HsPParen p1) (HsPParen p2) = alphaEq' p1 p2
    alphaEq' (HsPAsPat v1 p1) (HsPAsPat v2 p2) = do
        alphaEq' (convertName v1 :: Text) (convertName v2)
        alphaEq' p1 p2
    alphaEq' HsPWildCard HsPWildCard = return ()
    alphaEq' p1 p2 = throwError $ unlines [ "Pattern mismatch:", synPrint p1, "vs", synPrint p2 ]
instance AlphaEq HsRhs where
    alphaEq' (HsUnGuardedRhs e1) (HsUnGuardedRhs e2) = alphaEq' e1 e2
    alphaEq' r1 r2 = throwError $ unlines [ "RHS mismatch:", synPrint r1, "vs", synPrint r2 ]
instance AlphaEq HsExp where
    alphaEq' (HsVar v1) (HsVar v2) = alphaEq' (convertName v1 :: Text) (convertName v2)
    alphaEq' (HsCon c1) (HsCon c2) = alphaEq' (HsVar c1) (HsVar c2)
    alphaEq' (HsLit l1) (HsLit l2) =
        unless (l1 == l2) $ throwError $ "Literal exp mismatch: " <> synPrint l1 <> " " <> synPrint l2
    alphaEq' (HsApp e1a e1b) (HsApp e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (HsInfixApp e1a op1 e1b) (HsInfixApp e2a op2 e2b) = alphaEq' e1a e2a >> alphaEq' op1 op2 >> alphaEq' e1b e2b
    alphaEq' (HsNegApp e1) (HsNegApp e2) = alphaEq' e1 e2
    alphaEq' (HsLambda _ ps1 e1) (HsLambda _ ps2 e2) = alphaEq' ps1 ps2 >> alphaEq' e1 e2
    alphaEq' (HsLet ds1 e1) (HsLet ds2 e2) = alphaEq' ds1 ds2 >> alphaEq' e1 e2
    alphaEq' (HsIf e1a e1b e1c) (HsIf e2a e2b e2c) = alphaEq' [e1a, e1b, e1c] [e2a, e2b, e2c]
    alphaEq' (HsTuple es1) (HsTuple es2) = alphaEq' es1 es2
    alphaEq' (HsList es1) (HsList es2) = alphaEq' es1 es2
    alphaEq' (HsCase scrut1 as1) (HsCase scrut2 as2) = alphaEq' scrut1 scrut2 >> alphaEq' as1 as2
    alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    alphaEq' (HsExpTypeSig _ e1 t1) (HsExpTypeSig _ e2 t2) = alphaEq' e1 e2 >> alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "Expression mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq HsAlt where
    alphaEq' (HsAlt _ pat1 as1 wheres1) (HsAlt _ pat2 as2 wheres2) = do
        alphaEq' pat1 pat2
        alphaEq' as1 as2
        alphaEq' wheres1 wheres2
instance AlphaEq HsGuardedAlts where
    alphaEq' (HsUnGuardedAlt e1) (HsUnGuardedAlt e2) = alphaEq' e1 e2
    alphaEq' (HsGuardedAlts guards1) (HsGuardedAlts guards2) = alphaEq' guards1 guards2
    alphaEq' a1 a2 = throwError $ unlines [ "Alt mismatch:", showt a1, showt a2 ]
instance AlphaEq HsGuardedAlt where
    alphaEq' (HsGuardedAlt _ scrut1 e1) (HsGuardedAlt _ scrut2 e2) = alphaEq' scrut1 scrut2 >> alphaEq' e1 e2
instance AlphaEq HsType where
    alphaEq' (HsTyFun t1a t1b) (HsTyFun t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyTuple ts1) (HsTyTuple ts2) = alphaEq' ts1 ts2
    alphaEq' (HsTyApp t1a t1b) (HsTyApp t2a t2b) = alphaEq' t1a t2a >> alphaEq' t1b t2b
    alphaEq' (HsTyVar v1) (HsTyVar v2) = alphaEq' (convertName v1 :: Text) (convertName v2)
    alphaEq' (HsTyCon v1) (HsTyCon v2) =
        unless (v1 == v2) $ throwError $ "Name mismatch: " <> synPrint v1 <> " vs " <> synPrint v2
    alphaEq' t1 t2 = throwError $ "Type mismatch: " <> synPrint t1 <> " vs " <> synPrint t2
instance AlphaEq HsQualType where
    alphaEq' (HsQualType c1 t1) (HsQualType c2 t2) = alphaEq' c1 c2 >> alphaEq' t1 t2
instance AlphaEq HsBangType where
    alphaEq' (HsBangedTy t1) (HsBangedTy t2) = alphaEq' t1 t2
    alphaEq' (HsUnBangedTy t1) (HsUnBangedTy t2) = alphaEq' t1 t2
    alphaEq' t1 t2 = throwError $ unlines ["Banged type mismatch", showt t1, "vs", showt t2]


-- ILA instances
instance (AlphaEq a, Ord a) => AlphaEq (ILA.Binding a) where
    alphaEq' (ILA.NonRec v1 e1) (ILA.NonRec v2 e2) = alphaEq' v1 v2 >> alphaEq' e1 e2
    alphaEq' (ILA.Rec m1) (ILA.Rec m2) = alphaEq' m1 m2
    alphaEq' b1 b2 = throwError $ unlines [ "Binding mismatch:", showt b1, "vs", showt b2 ]
instance AlphaEq a => AlphaEq (ILA.Alt a) where
    alphaEq' (ILA.Alt ac1 vs1 e1) (ILA.Alt ac2 vs2 e2) = alphaEq' ac1 ac2 >> alphaEq' vs1 vs2 >> alphaEq' e1 e2
instance AlphaEq ILA.Literal where
    alphaEq' (ILA.LiteralInt i1) (ILA.LiteralInt i2) =
        unless (i1 == i2) $ throwError $ "Integer literal mismatch:" <> showt i1 <> " vs " <> showt i2
    alphaEq' (ILA.LiteralFrac f1) (ILA.LiteralFrac f2) =
        unless (f1 == f2) $ throwError $ "Rational literal mismatch:" <> showt f1 <> " vs " <> showt f2
    alphaEq' (ILA.LiteralChar c1) (ILA.LiteralChar c2) =
        unless (c1 == c2) $ throwError $ "Character literal mismatch:" <> showt c1 <> " vs " <> showt c2
    alphaEq' (ILA.LiteralString s1) (ILA.LiteralString s2) =
        unless (s1 == s2) $ throwError $ "Text literal mismatch:" <> showt s1 <> " vs " <> showt s2
    alphaEq' l1 l2 = throwError $ "Literal mismatch:" <> showt l1 <> " vs " <> showt l2
instance AlphaEq ILA.AltConstructor where
    alphaEq' (ILA.DataCon v1) (ILA.DataCon v2) = alphaEq' v1 v2
    alphaEq' (ILA.LitCon l1) (ILA.LitCon l2) = alphaEq' l1 l2
    alphaEq' ILA.Default ILA.Default = return ()
    alphaEq' c1 c2 = throwError $ unlines [ "Alt constructor mismatch:", showt c1, "vs", showt c2 ]
instance AlphaEq ILA.Expr where
    alphaEq' (ILA.Var n1 t1) (ILA.Var n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (ILA.Con n1 t1) (ILA.Con n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (ILA.Lit l1 t1) (ILA.Lit l2 t2) = alphaEq' l1 l2 >> alphaEq' t1 t2
    alphaEq' (ILA.App e1a e1b) (ILA.App e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (ILA.Lam v1 t1 e1) (ILA.Lam v2 t2 e2) = alphaEq' v1 v2 >> alphaEq' t1 t2 >> alphaEq' e1 e2
    alphaEq' (ILA.Let v1 t1 e1a e1b) (ILA.Let v2 t2 e2a e2b) = do
        alphaEq' v1 v2
        alphaEq' t1 t2
        alphaEq' e1a e2a
        alphaEq' e1b e2b
    alphaEq' (ILA.Case e1 vs1 as1) (ILA.Case e2 vs2 as2) = alphaEq' e1 e2 >> alphaEq' vs1 vs2 >> alphaEq' as1 as2
    alphaEq' (ILA.Type t1) (ILA.Type t2) = alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "ILA Expression mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq ILAANF.AnfTrivial where
    alphaEq' (ILAANF.Var n1 t1) (ILAANF.Var n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (ILAANF.Con n1 t1) (ILAANF.Con n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (ILAANF.Lit l1 t1) (ILAANF.Lit l2 t2) = alphaEq' l1 l2 >> alphaEq' t1 t2
    alphaEq' (ILAANF.Type t1) (ILAANF.Type t2) = alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfTrivial mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq ILAANF.AnfApplication where
    alphaEq' (ILAANF.App e1a e1b) (ILAANF.App e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (ILAANF.TrivApp e1) (ILAANF.TrivApp e2) = alphaEq' e1 e2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfApplication mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq ILAANF.AnfComplex where
    alphaEq' (ILAANF.Let v1 t1 e1a e1b) (ILAANF.Let v2 t2 e2a e2b) = do
        alphaEq' v1 v2
        alphaEq' t1 t2
        alphaEq' e1a e2a
        alphaEq' e1b e2b
    alphaEq' (ILAANF.Case e1 vs1 as1) (ILAANF.Case e2 vs2 as2) = alphaEq' e1 e2 >> alphaEq' vs1 vs2 >> alphaEq' as1 as2
    alphaEq' (ILAANF.Trivial e1) (ILAANF.Trivial e2) = alphaEq' e1 e2
    alphaEq' (ILAANF.CompApp e1) (ILAANF.CompApp e2) = alphaEq' e1 e2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfComplex mismatch:", showt e1, "vs", showt e2 ]
instance AlphaEq ILAANF.AnfRhs where
    alphaEq' (ILAANF.Lam v1 t1 e1) (ILAANF.Lam v2 t2 e2) = alphaEq' v1 v2 >> alphaEq' t1 t2 >> alphaEq' e1 e2
    alphaEq' (ILAANF.Complex c1) (ILAANF.Complex c2) = alphaEq' c1 c2
    alphaEq' e1 e2 = throwError $ unlines [ "AnfRhs mismatch:", showt e1, "vs", showt e2 ]


stripModuleParens :: HsModule -> HsModule
stripModuleParens (HsModule a b c d e) = HsModule a b c d (stripDeclsParens e)
stripDeclParens :: HsDecl -> HsDecl
stripDeclParens (HsPatBind l p r ds) = HsPatBind l p (stripRhsParens r) (stripDeclsParens ds)
stripDeclParens _                    = error "Unsupported declaration in paren strip"
stripDeclsParens :: [HsDecl] -> [HsDecl]
stripDeclsParens = map stripDeclParens
stripRhsParens :: HsRhs -> HsRhs
stripRhsParens (HsUnGuardedRhs e) = HsUnGuardedRhs (stripExpParens e)
stripRhsParens _                  = error "Unsupported RHS in paren strip"
stripExpParens :: HsExp -> HsExp
stripExpParens (HsParen e)           = stripExpParens e
stripExpParens (HsApp e1 e2)         = HsApp (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsInfixApp e1 op e2) = HsInfixApp (stripExpParens e1) op (stripExpParens e2)
stripExpParens (HsNegApp e)          = HsNegApp (stripExpParens e)
stripExpParens (HsLambda l ps e)     = HsLambda l ps (stripExpParens e)
stripExpParens (HsIf c e1 e2)        = HsIf (stripExpParens c) (stripExpParens e1) (stripExpParens e2)
stripExpParens (HsLet ds e)          = HsLet ds (stripExpParens e)
stripExpParens (HsTuple es)          = HsTuple (map stripExpParens es)
stripExpParens (HsList es)           = HsList (map stripExpParens es)
stripExpParens (HsExpTypeSig l e t)  = HsExpTypeSig l (stripExpParens e) t
stripExpParens e                     = e
