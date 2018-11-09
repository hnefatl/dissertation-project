{-# Language GeneralizedNewtypeDeriving, TupleSections, MultiParamTypeClasses, FlexibleContexts #-}

module Preprocessor.Renamer where

import Language.Haskell.Syntax
import Data.Hashable
import Data.Foldable
import Control.Monad.State
import Control.Monad.Except
import Text.Printf
import qualified Data.Set as S
import qualified Data.HashMap.Strict as M

import ExtraDefs

newtype Name = Name Id deriving (Eq, Ord, Show, Hashable)
newtype UniqueName = UniqueName Id deriving (Eq, Ord, Show, Hashable)

data RenamerState = RenamerState
      -- Used to generate unique variable names
    { variableCounter :: Int
      -- Mappings from a variable name to a stack of unique names. The stack is to facilitate nesting.
    , bindings :: M.HashMap Name [UniqueName] 
      -- A reverse mapping from unique names to their original variable name: useful for printing error messages.
    , reverseMapping :: M.HashMap UniqueName Name }

newtype Renamer a = Deduper (ExceptT String (State RenamerState) a)
    deriving (Applicative, Functor, Monad, MonadError String, MonadState RenamerState)
instance NameGenerator Renamer UniqueName where
    freshName = do
        counter <- (1 +) <$> gets variableCounter
        modify (\s -> s { variableCounter = counter })
        return (UniqueName $ Id $ "v" ++ show counter)

type Rename a = a -> Renamer a

disjointUnion :: (MonadError String m, Ord a, Show a) => S.Set a -> S.Set a -> m (S.Set a)
disjointUnion s1 s2 = if S.null inter then return (S.union s1 s2) else throwError err
    where inter = S.intersection s1 s2
          err = printf "Duplicate binding names in same level: %s" (show $ S.toList inter)
disjointUnions :: (MonadError String m, Foldable f, Ord a, Show a) => f (S.Set a) -> m (S.Set a)
disjointUnions = foldlM disjointUnion S.empty

createMapping :: S.Set Name -> Renamer ()
createMapping names = do
    when (containsDuplicates names) (throwError "Term")
    mapping <- M.fromList <$> mapM (\name -> (name,) . pure <$> freshName) (S.toList names)
    modify (\s -> s { bindings = M.unionWith (++) mapping (bindings s) })

getPatBoundNames :: MonadError String m => HsPat -> m (S.Set Name)
getPatBoundNames (HsPVar v) = return $ S.singleton (Name $ toId v)
getPatBoundNames (HsPLit _) = return S.empty
getPatBoundNames HsPWildCard = return S.empty
getPatBoundNames (HsPNeg p) = getPatBoundNames p
getPatBoundNames (HsPParen p) = getPatBoundNames p
getPatBoundNames (HsPIrrPat p) = getPatBoundNames p
getPatBoundNames (HsPAsPat v p) = disjointUnion (S.singleton $ Name $ toId v) =<< getPatBoundNames p
getPatBoundNames (HsPInfixApp p1 _ p2) = do
    ns1 <- getPatBoundNames p1
    ns2 <- getPatBoundNames p2
    disjointUnion ns1 ns2
getPatBoundNames (HsPApp _ ps) = disjointUnions =<< mapM getPatBoundNames ps
getPatBoundNames (HsPTuple ps) = disjointUnions =<< mapM getPatBoundNames ps
getPatBoundNames (HsPList ps) = disjointUnions =<< mapM getPatBoundNames ps
getPatBoundNames (HsPRec _ _) = throwError "Pattern records not supported"

getPatsBoundNames :: MonadError String m => [HsPat] -> m (S.Set Name)
getPatsBoundNames ps = disjointUnions =<< mapM getPatBoundNames ps

getDeclBoundNames :: MonadError String m => HsDecl -> m (S.Set Name)
getDeclBoundNames (HsPatBind _ pat _ _) = getPatBoundNames pat
getDeclBoundNames (HsFunBind matches) = do
    let (funNames, pats') = unzip $ map (\(HsMatch _ name ps _ _) -> (toId name, ps)) matches
        pats = concat pats'
        funName = head funNames
        allNamesMatch = all (== funName) funNames
    names <- getPatsBoundNames pats
    if allNamesMatch then return $ S.insert (Name funName) names else throwError "Mismatched function names"
getDeclBoundNames _ = throwError "Declaration not supported"

getDeclsBoundNames :: MonadError String m => [HsDecl] -> m (S.Set Name)
getDeclsBoundNames ds = disjointUnions =<< mapM getDeclBoundNames ds

renameModule :: Rename HsModule
renameModule (HsModule a b c d e) = HsModule a b c d <$> renameDecls e

renameDecls :: Rename [HsDecl]
renameDecls decls = do
    boundVars <- getDeclsBoundNames decls
    createMapping boundVars
    undefined

renamePat :: Rename HsPat
renamePat = undefined