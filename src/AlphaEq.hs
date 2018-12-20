{-# Language GeneralizedNewtypeDeriving, FlexibleContexts, FlexibleInstances, LambdaCase #-}

module AlphaEq where

import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Language.Haskell.Syntax

import ExtraDefs
import Names

newtype AlphaEqM a = AlphaEqM { inner :: State (M.Map String String) a }
    deriving (Functor, Applicative, Monad, MonadState (M.Map String String))

class AlphaEq a where
    alphaEq' :: a -> a -> AlphaEqM Bool
alphaEqs' :: AlphaEq a => [a] -> [a] -> AlphaEqM Bool
alphaEqs' [] [] = return True
alphaEqs' (x:xs) (y:ys) = (&&) <$> alphaEq' x y <*> alphaEqs' xs ys
alphaEqs' _ _ = return False

alphaEq :: AlphaEq a => a -> a -> Bool
alphaEq x y = evalState (inner $ alphaEq' x y) M.empty

runAlphaEq :: AlphaEq a => a -> a -> (Bool, M.Map String String)
runAlphaEq x y = runState (inner $ alphaEq' x y) M.empty

instance AlphaEq String where
    alphaEq' s1 s2
        | s1 == s2 = return True
        | otherwise = (,) <$> gets (M.lookup s1) <*> gets (M.lookup s2) >>= \case
            -- Neither's been renamed before, add renames to them both
            (Nothing, Nothing) -> do
                modify (M.union $ M.fromList [(s1, s2), (s2, s1)])
                return True
            -- Both have been renamed before, check if they've been renamed to each other
            (Just x, Just y) -> return $ x == s2 && y == s1
            -- One's been renamed but the other hasn't: they can't be renamed to the same thing
            (_, _) -> return False
-- TODO(kc506): When we swap String for Text, add an instance for lists and find all the places we can use that.
instance (Ord a, AlphaEq a) => AlphaEq (S.Set a) where
    alphaEq' s1 s2
        | S.null s1 && S.null s2 = return True -- Both empty, both alpha equivalent
        | S.null s1 || S.null s2 = return False -- Different sizes, can't be alpha equivalent
        | otherwise = do -- Find an element from a set that's alpha-eq to one from the other set, remove it, recurse
            let x = S.findMin s1 -- Arbitrary element from first set
            findM (alphaEq' x) (S.toList s2) >>= \case -- Find an alpha-eq element from the other set
                Nothing -> return False -- No matching element
                Just y -> alphaEq' (S.delete x s1) (S.delete y s2) -- Found an equivalent element, remove and recurse


-- Have to define these instances here otherwise splitting names/alphaeq instances becomes painful (cyclic)
instance AlphaEq TypeVariableName where
    alphaEq' (TypeVariableName s1) (TypeVariableName s2) = alphaEq' s1 s2

-- Have to define these instances here because anywhere else would produce orphan instances...
instance AlphaEq HsModule where
    alphaEq' (HsModule _ _ _ _ ds1) (HsModule _ _ _ _ ds2) = alphaEqs' ds1 ds2
instance AlphaEq HsDecl where
    alphaEq' (HsPatBind _ pat1 rhs1 ds1) (HsPatBind _ pat2 rhs2 ds2) = do
        p <- alphaEq' pat1 pat2
        r <- alphaEq' rhs1 rhs2
        ds <- alphaEqs' ds1 ds2
        return $ p && r && ds
    alphaEq' _ _ = return False
instance AlphaEq HsPat where
    alphaEq' (HsPVar n1) (HsPVar n2) = alphaEq' (convertName n1 :: String) (convertName n2)
    alphaEq' (HsPLit l1) (HsPLit l2) = return $ l1 == l2
    alphaEq' (HsPApp con1 ps1) (HsPApp con2 ps2) = do
        c <- alphaEq' (convertName con1 :: String) (convertName con2) 
        ps <- alphaEqs' ps1 ps2
        return $ c && ps
    alphaEq' (HsPTuple ps1) (HsPTuple ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPList ps1) (HsPList ps2) = alphaEqs' ps1 ps2
    alphaEq' (HsPParen p1) (HsPParen p2) = alphaEq' p1 p2
    alphaEq' (HsPAsPat v1 p1) (HsPAsPat v2 p2) = do
        v <- alphaEq' (convertName v1 :: String) (convertName v2)
        p <- alphaEq' p1 p2
        return $ v && p
    alphaEq' HsPWildCard HsPWildCard = return True
    alphaEq' _ _ = return False
instance AlphaEq HsRhs where
    alphaEq' (HsUnGuardedRhs e1) (HsUnGuardedRhs e2) = alphaEq' e1 e2
    alphaEq' _ _ = return False
instance AlphaEq HsExp where
    alphaEq' (HsVar v1) (HsVar v2) = alphaEq' (convertName v1 :: String) (convertName v2)
    alphaEq' (HsCon c1) (HsCon c2) = alphaEq' (HsVar c1) (HsVar c2)
    alphaEq' (HsLit l1) (HsLit l2) = return $ l1 == l2
    alphaEq' (HsApp e1a e1b) (HsApp e2a e2b) = (&&) <$> alphaEq' e1a e2a <*> alphaEq' e1b e2b
    alphaEq' (HsNegApp e1) (HsNegApp e2) = alphaEq' e1 e2
    alphaEq' (HsLambda _ ps1 e1) (HsLambda _ ps2 e2) = (&&) <$> alphaEqs' ps1 ps2 <*> alphaEq' e1 e2
    alphaEq' (HsLet ds1 e1) (HsLet ds2 e2) = (&&) <$> alphaEqs' ds1 ds2 <*> alphaEq' e1 e2
    alphaEq' (HsIf e1a e1b e1c) (HsIf e2a e2b e2c) = and <$> zipWithM alphaEq' [e1a, e1b, e1c] [e2a, e2b, e2c]
    alphaEq' (HsTuple es1) (HsTuple es2) = alphaEqs' es1 es2
    alphaEq' (HsList es1) (HsList es2) = alphaEqs' es1 es2
    alphaEq' (HsParen e1) (HsParen e2) = alphaEq' e1 e2
    alphaEq' _ _ = return False