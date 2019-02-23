{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Utility functions for getting variable names from the parse tree
module Preprocessor.ContainedNames where

import           BasicPrelude
import           Control.Monad.Except    (MonadError, throwError)
import           Data.Foldable           (foldlM)
import qualified Data.Map.Strict         as M
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           Language.Haskell.Syntax
import           TextShow                (showt)

import           ExtraDefs               (synPrint)
import           Names
import           Tuples                  (makeTupleName)
import           Typechecker.Types       (Qualified(..), Type(..), TypePredicate(..), TypeVariable(..))


type VariableConflictInfo = M.Map VariableName (S.Set ConflictInfo)
singleton :: VariableName -> ConflictInfo -> VariableConflictInfo
singleton v = M.singleton v . S.singleton

disjointInsert :: MonadError Text m => VariableName -> ConflictInfo -> VariableConflictInfo -> m VariableConflictInfo
disjointInsert v c m = case M.lookup v m of
    Nothing -> return $ M.insert v (S.singleton c) m
    Just cs
        | any (conflict c) cs -> throwError $ "Conflict over bound symbol " <> showt v
        | otherwise -> return $ M.insert v (S.insert c cs) m

disjointInserts :: MonadError Text m => VariableName -> S.Set ConflictInfo -> VariableConflictInfo -> m VariableConflictInfo
disjointInserts v cs m = foldlM (flip $ disjointInsert v) m cs

disjointUnion :: MonadError Text m => VariableConflictInfo -> VariableConflictInfo -> m VariableConflictInfo
disjointUnion m1 m2 = foldM (\m (v, cs) -> disjointInserts v cs m) m2 (M.toList m1)

disjointUnions :: (MonadError Text m, Foldable f) => f VariableConflictInfo -> m VariableConflictInfo
disjointUnions = foldlM disjointUnion M.empty

-- Describes how different names conflict with each other
data ConflictInfo = SymDef | SymType | ClassSymDef Text | ClassSymType Text | InstSymDef Text Type
    deriving (Eq, Ord, Show)

-- |Predicate should return True iff a bound variable with the first conflict info would conflict with a bound variable
-- in the same scope with the second conflict info. Predicate must be symmetric otherwise the world will end.
conflict :: ConflictInfo -> ConflictInfo -> Bool
conflict SymDef SymDef                         = True
conflict SymDef ClassSymDef{}                  = True
conflict SymDef ClassSymType{}                 = True
conflict SymDef InstSymDef{}                   = True
conflict SymType ClassSymDef{}                 = True
conflict SymType ClassSymType{}                = True
conflict SymType InstSymDef{}                  = True
conflict ClassSymDef{} SymDef                  = True
conflict ClassSymDef{} SymType                 = True
conflict (ClassSymDef c1) (ClassSymDef c2)     = c1 /= c2
conflict ClassSymType{} SymDef                 = True
conflict ClassSymType{} SymType                = True
conflict (ClassSymType c1) (ClassSymType c2)   = c1 /= c2
conflict InstSymDef{} SymDef                   = True
conflict InstSymDef{} SymType                  = True
conflict (InstSymDef c1 t1) (InstSymDef c2 t2) = (c1 == c2) /= (t1 == t2) -- xor
conflict _ _                                   = False

class HasBoundVariables a where
    -- Returns a set of pairs of a bound variable along with True iff the variable can conflict with other variables
    -- defined in the same horizontal scope.
    getBoundVariablesAndConflicts :: MonadError Text m => a -> m VariableConflictInfo
    getBoundVariables :: MonadError Text m => a -> m (S.Set VariableName)
    getBoundVariables = fmap M.keysSet . getBoundVariablesAndConflicts
class HasFreeVariables a where
    getFreeVariables :: MonadError Text m => a -> m (S.Set VariableName)
class HasFreeTypeVariables a where
    getFreeTypeVariables :: a -> S.Set TypeVariableName
class HasFreeTypeConstants a where
    getFreeTypeConstants :: a -> S.Set TypeVariableName
class HasBoundTypeConstants a where
    getBoundTypeConstants :: a -> S.Set TypeVariableName

instance {-# Overlappable #-} (Traversable t, HasBoundVariables a) => HasBoundVariables (t a) where
    getBoundVariablesAndConflicts = disjointUnions <=< mapM getBoundVariablesAndConflicts
instance {-# Overlappable #-} (Traversable t, HasFreeVariables a) => HasFreeVariables (t a) where
    getFreeVariables = fmap S.unions . mapM getFreeVariables
instance {-# Overlappable #-} (Functor f, Foldable f, HasFreeTypeVariables a) => HasFreeTypeVariables (f a) where
    getFreeTypeVariables = S.unions . fmap getFreeTypeVariables
instance {-# Overlappable #-} (Functor f, Foldable f, HasFreeTypeConstants a) => HasFreeTypeConstants (f a) where
    getFreeTypeConstants = S.unions . fmap getFreeTypeConstants
instance {-# Overlappable #-} (Functor f, Foldable f, HasBoundTypeConstants a) => HasBoundTypeConstants (f a) where
    getBoundTypeConstants = S.unions . fmap getBoundTypeConstants

instance HasBoundVariables HsDecl where
    getBoundVariablesAndConflicts (HsPatBind _ pat _ _) = getBoundVariablesAndConflicts pat
    getBoundVariablesAndConflicts (HsFunBind matches) = do
        -- Specifically mapM over the matches so we don't disjoint union the bound match variables: they all bind the
        -- same name so we'll get an error.
        names <- S.toList . S.unions <$> mapM getBoundVariables matches
        let funName = head names
            allNamesMatch = all (== funName) names
        if allNamesMatch then return $ singleton funName SymDef else throwError "Mismatched function names"
    getBoundVariablesAndConflicts (HsTypeSig _ names _) =
        disjointUnions $ map (\n -> singleton (convertName n) SymType) names
    getBoundVariablesAndConflicts (HsDataDecl _ _ _ _ conDecls _) = getBoundVariablesAndConflicts conDecls
    getBoundVariablesAndConflicts (HsClassDecl _ _ name _ decls) =
        M.map (S.map move) <$> getBoundVariablesAndConflicts decls
        where move SymDef  = ClassSymDef $ convertName name
              move SymType = ClassSymType $ convertName name
              move c       = error $ unpack ("Invalid conflict type in class " <> convertName name <> ": ") <> show c
    getBoundVariablesAndConflicts (HsInstDecl _ _ name _ decls) =
        M.map (S.map move) <$> getBoundVariablesAndConflicts decls
        where move SymDef = ClassSymDef $ convertName name
              move c = error $ unpack ("Invalid conflict type in instance " <> convertName name <> ": ") <> show c
    getBoundVariablesAndConflicts d = throwError $ unlines ["Declaration not supported:", synPrint d]
instance HasBoundVariables HsMatch where
    getBoundVariablesAndConflicts (HsMatch _ n _ _ _) = return $ singleton (convertName n) SymDef
instance HasBoundVariables HsConDecl where
    getBoundVariablesAndConflicts (HsConDecl _ n _) = return $ singleton (convertName n) SymDef
    getBoundVariablesAndConflicts (HsRecDecl _ n _) = return $ singleton (convertName n) SymDef
instance HasBoundVariables HsPat where
    getBoundVariablesAndConflicts (HsPVar v)            = return $ singleton (convertName v) SymDef
    getBoundVariablesAndConflicts HsPLit{}              = return M.empty
    getBoundVariablesAndConflicts HsPWildCard           = return M.empty
    getBoundVariablesAndConflicts (HsPNeg p)            = getBoundVariablesAndConflicts p
    getBoundVariablesAndConflicts (HsPParen p)          = getBoundVariablesAndConflicts p
    getBoundVariablesAndConflicts (HsPIrrPat p)         = getBoundVariablesAndConflicts p
    getBoundVariablesAndConflicts (HsPAsPat v p)        = disjointInsert (convertName v) SymDef =<< getBoundVariablesAndConflicts p
    getBoundVariablesAndConflicts (HsPInfixApp p1 _ p2) = getBoundVariablesAndConflicts [p1, p2]
    getBoundVariablesAndConflicts (HsPApp _ ps)         = getBoundVariablesAndConflicts ps
    getBoundVariablesAndConflicts (HsPTuple ps)         = getBoundVariablesAndConflicts ps
    getBoundVariablesAndConflicts (HsPList ps)          = getBoundVariablesAndConflicts ps
    getBoundVariablesAndConflicts HsPRec{}              = throwError "Pattern records not supported"

instance HasFreeVariables HsDecl where
    getFreeVariables (HsPatBind _ pat rhs _)    = S.union <$> getFreeVariables pat <*> getFreeVariables rhs
    getFreeVariables (HsClassDecl _ _ _ _ args) = S.unions <$> mapM getFreeVariables args
    getFreeVariables (HsInstDecl _ _ _ _ args) = S.unions <$> mapM getFreeVariables args
    getFreeVariables HsTypeSig{}                = return S.empty
    getFreeVariables HsDataDecl{}               = return S.empty
    getFreeVariables (HsFunBind matches)        = do
        names <- S.toList . S.unions <$> mapM getBoundVariables matches
        let funName = head names
            allNamesMatch = all (== funName) names
        if allNamesMatch then getFreeVariables matches else throwError "Mismatched function names"
    getFreeVariables _                          = throwError "Not supported"
instance HasFreeVariables HsMatch where
    getFreeVariables (HsMatch _ name pats rhs _) = do
        rhsFree <- getFreeVariables rhs
        patFree <- getFreeVariables pats
        patBound <- getBoundVariables pats
        return $ S.difference (S.union rhsFree patFree) $ S.insert (convertName name) patBound
instance HasFreeVariables HsRhs where
    getFreeVariables (HsUnGuardedRhs e) = getFreeVariables e
    getFreeVariables (HsGuardedRhss _)  = throwError "Guarded rhss not supported"
instance HasFreeVariables HsExp where
    getFreeVariables (HsVar name)          = return $ S.singleton $ convertName name
    getFreeVariables (HsCon name)          = return $ S.singleton $ convertName name
    getFreeVariables HsLit{}               = return S.empty
    getFreeVariables (HsInfixApp e1 op e2) = S.insert (convertName op) <$> getFreeVariables [e1, e2]
    getFreeVariables (HsApp e1 e2)         = S.union <$> getFreeVariables e1 <*> getFreeVariables e2
    getFreeVariables (HsNegApp e)          = getFreeVariables e
    getFreeVariables (HsLambda _ pats e)   = S.difference <$> (S.union <$> getFreeVariables e <*> getFreeVariables pats) <*> getBoundVariables pats
    getFreeVariables (HsLet ds e)          = S.union <$> getFreeVariables ds <*> getFreeVariables e
    getFreeVariables (HsCase scrut alts)   = S.union <$> getFreeVariables scrut <*> getFreeVariables alts
    getFreeVariables (HsIf e1 e2 e3)       = getFreeVariables [e1, e2, e3]
    getFreeVariables (HsTuple es)          = S.insert (VariableName $ makeTupleName $ length es) <$> getFreeVariables es
    getFreeVariables (HsList es)           = S.insert "[]" <$> getFreeVariables es
    getFreeVariables (HsParen e)           = getFreeVariables e
    getFreeVariables (HsExpTypeSig _ e _)  = getFreeVariables e
    getFreeVariables e                     = throwError $ pack $ "Unsupported expression " <> show e
instance HasFreeVariables HsPat where
    getFreeVariables HsPVar{}                = return S.empty
    getFreeVariables HsPLit{}                = return S.empty
    getFreeVariables HsPWildCard             = return S.empty
    getFreeVariables (HsPNeg p)              = getFreeVariables p
    getFreeVariables (HsPParen p)            = getFreeVariables p
    getFreeVariables (HsPIrrPat p)           = getFreeVariables p
    getFreeVariables (HsPAsPat _ p)          = getFreeVariables p
    getFreeVariables (HsPInfixApp p1 con p2) = S.insert (convertName con) <$> getFreeVariables [p1, p2]
    getFreeVariables (HsPApp con ps)         = S.insert (convertName con) <$> getFreeVariables ps
    getFreeVariables (HsPTuple ps)           = S.insert (VariableName $ makeTupleName $ length ps) <$> getFreeVariables ps
    getFreeVariables (HsPList ps)            = S.insert "[]" <$> getFreeVariables ps
    getFreeVariables HsPRec{}                = throwError "Pattern records not supported"
instance HasFreeVariables HsAlt where
    getFreeVariables (HsAlt _ pat as ds) = do
        wheresFree <- getFreeVariables ds
        altsFree <- getFreeVariables as
        patBound <- getBoundVariables pat
        patFree <- getFreeVariables pat
        return $ S.unions [wheresFree, patFree, S.difference altsFree patBound]
instance HasFreeVariables HsGuardedAlts where
    getFreeVariables (HsUnGuardedAlt e) = getFreeVariables e
    getFreeVariables (HsGuardedAlts as) = getFreeVariables as
instance HasFreeVariables HsGuardedAlt where
    getFreeVariables (HsGuardedAlt _ e1 e2) = S.union <$> getFreeVariables e1 <*> getFreeVariables e2

instance HasFreeTypeVariables HsQualType where
    getFreeTypeVariables (HsQualType quals t) = S.union (getFreeTypeVariables quals) (getFreeTypeVariables t)
instance HasFreeTypeVariables HsType where
    getFreeTypeVariables (HsTyFun t1 t2) = S.union (getFreeTypeVariables t1) (getFreeTypeVariables t2)
    getFreeTypeVariables (HsTyTuple ts)  = getFreeTypeVariables ts
    getFreeTypeVariables (HsTyApp t1 t2) = S.union (getFreeTypeVariables t1) (getFreeTypeVariables t2)
    getFreeTypeVariables (HsTyVar n)     = S.singleton $ convertName n
    getFreeTypeVariables HsTyCon{}       = S.empty

instance HasFreeTypeVariables TypeVariable where
    getFreeTypeVariables (TypeVariable v _) = S.singleton v
instance HasFreeTypeVariables Type where
    getFreeTypeVariables (TypeVar v)       = getFreeTypeVariables v
    getFreeTypeVariables TypeCon{}         = S.empty
    getFreeTypeVariables (TypeApp t1 t2 _) = S.union (getFreeTypeVariables t1) (getFreeTypeVariables t2)
instance HasFreeTypeVariables a => HasFreeTypeVariables (Qualified a) where
    getFreeTypeVariables (Qualified quals t) = S.unions $ getFreeTypeVariables t:map getFreeTypeVariables (S.toList quals)
instance HasFreeTypeVariables TypePredicate where
    getFreeTypeVariables (IsInstance _ t) = getFreeTypeVariables t

instance HasFreeTypeConstants HsDecl where
    getFreeTypeConstants (HsPatBind _ pat rhs wheres) =
        S.unions [getFreeTypeConstants pat, getFreeTypeConstants rhs, getFreeTypeConstants wheres]
    getFreeTypeConstants (HsFunBind matches) = getFreeTypeConstants matches
    getFreeTypeConstants (HsClassDecl _ _ _ _ ds) = getFreeTypeConstants ds
    getFreeTypeConstants (HsInstDecl _ _ name ts ds) =
        S.unions [S.singleton $ convertName name, getFreeTypeConstants ts, getFreeTypeConstants ds]
    getFreeTypeConstants (HsTypeSig _ _ t)        = getFreeTypeConstants t
    getFreeTypeConstants (HsDataDecl _ _ _ _ cons _) = getFreeTypeConstants cons
    getFreeTypeConstants _                        = S.empty
instance HasFreeTypeConstants HsConDecl where
    getFreeTypeConstants (HsConDecl _ _ ts) = getFreeTypeConstants ts
    getFreeTypeConstants HsRecDecl{}        = error "recdecl"
instance HasFreeTypeConstants HsBangType where
    getFreeTypeConstants (HsBangedTy t)   = getFreeTypeConstants t
    getFreeTypeConstants (HsUnBangedTy t) = getFreeTypeConstants t
instance HasFreeTypeConstants HsRhs where
    getFreeTypeConstants (HsUnGuardedRhs e)   = getFreeTypeConstants e
    getFreeTypeConstants (HsGuardedRhss rhss) = getFreeTypeConstants rhss
instance HasFreeTypeConstants HsGuardedRhs where
    getFreeTypeConstants (HsGuardedRhs _ e1 e2) = getFreeTypeConstants [e1, e2]
instance HasFreeTypeConstants HsAlt where
    getFreeTypeConstants (HsAlt _ pat alts wheres) = S.unions [getFreeTypeConstants pat, getFreeTypeConstants alts, getFreeTypeConstants wheres]
instance HasFreeTypeConstants HsGuardedAlts where
    getFreeTypeConstants (HsUnGuardedAlt e) = getFreeTypeConstants e
    getFreeTypeConstants (HsGuardedAlts as) = getFreeTypeConstants as
instance HasFreeTypeConstants HsGuardedAlt where
    getFreeTypeConstants (HsGuardedAlt _ e1 e2) = getFreeTypeConstants [e1, e2]
instance HasFreeTypeConstants HsMatch where
    getFreeTypeConstants (HsMatch _ _ pats rhs wheres) = S.unions [getFreeTypeConstants pats, getFreeTypeConstants rhs, getFreeTypeConstants wheres]
instance HasFreeTypeConstants HsLiteral where
    -- For the sake of dependency analysis, we pretend literals have their typeclasses free so we compile those first
    getFreeTypeConstants HsChar{} = S.singleton "Char"
    getFreeTypeConstants HsString{} = S.singleton "String"
    getFreeTypeConstants HsInt{} = S.fromList ["Integer", "Num"]
    getFreeTypeConstants HsFrac{} = S.fromList ["Rational", "Num"]
    getFreeTypeConstants _ = error "Unsupported literal"
instance HasFreeTypeConstants HsExp where
    getFreeTypeConstants (HsLit l) = getFreeTypeConstants l
    getFreeTypeConstants (HsInfixApp e1 _ e2)  = S.union (getFreeTypeConstants e1) (getFreeTypeConstants e2)
    getFreeTypeConstants (HsApp e1 e2)         = S.union (getFreeTypeConstants e1) (getFreeTypeConstants e2)
    getFreeTypeConstants (HsNegApp e)          = getFreeTypeConstants e
    getFreeTypeConstants (HsLambda _ pats e)   = S.union (getFreeTypeConstants e) (getFreeTypeConstants pats)
    getFreeTypeConstants (HsLet ds e)          = S.union (getFreeTypeConstants ds) (getFreeTypeConstants e)
    getFreeTypeConstants (HsCase scrut alts)   = S.union (getFreeTypeConstants scrut) (getFreeTypeConstants alts)
    getFreeTypeConstants (HsIf e1 e2 e3)       = getFreeTypeConstants [e1, e2, e3]
    getFreeTypeConstants (HsTuple es)          = S.insert tupleCon (getFreeTypeConstants es)
        where tupleCon = TypeVariableName $ makeTupleName $ length es
    getFreeTypeConstants (HsList es)           = S.insert "[]" (getFreeTypeConstants es)
    getFreeTypeConstants (HsParen e)           = getFreeTypeConstants e
    getFreeTypeConstants (HsExpTypeSig _ e _)  = getFreeTypeConstants e
    getFreeTypeConstants _                     = S.empty
instance HasFreeTypeConstants HsPat where
    getFreeTypeConstants (HsPNeg p)              = getFreeTypeConstants p
    getFreeTypeConstants (HsPParen p)            = getFreeTypeConstants p
    getFreeTypeConstants (HsPIrrPat p)           = getFreeTypeConstants p
    getFreeTypeConstants (HsPAsPat _ p)          = getFreeTypeConstants p
    getFreeTypeConstants (HsPInfixApp p1 _ p2) = getFreeTypeConstants [p1, p2]
    getFreeTypeConstants (HsPApp _ ps)         = getFreeTypeConstants ps
    getFreeTypeConstants (HsPTuple ps)           = S.insert tupleCon (getFreeTypeConstants ps)
        where tupleCon = TypeVariableName $ makeTupleName $ length ps
    getFreeTypeConstants (HsPList ps)            = S.insert "[]" (getFreeTypeConstants ps)
    getFreeTypeConstants _                       = S.empty
instance HasFreeTypeConstants HsQualType where
    getFreeTypeConstants (HsQualType quals t) = S.union qualConsts (getFreeTypeConstants t)
        where qualConsts = S.unions $ map (S.singleton . convertName . fst) quals
instance HasFreeTypeConstants HsType where
    getFreeTypeConstants (HsTyFun t1 t2) = getFreeTypeConstants [t1, t2]
    getFreeTypeConstants (HsTyTuple ts)  =
        S.insert (TypeVariableName $ makeTupleName $ length ts) $ getFreeTypeConstants ts
    getFreeTypeConstants (HsTyApp t1 t2) = getFreeTypeConstants [t1, t2]
    getFreeTypeConstants HsTyVar{}       = S.empty
    getFreeTypeConstants (HsTyCon n)     = S.singleton $ convertName n

instance HasBoundTypeConstants HsDecl where
    getBoundTypeConstants (HsDataDecl _ _ name _ _ _)    = S.singleton $ convertName name
    getBoundTypeConstants (HsClassDecl _ _ name _ _)     = S.singleton $ convertName name
    getBoundTypeConstants (HsTypeDecl _ name _ _)        = S.singleton $ convertName name
    getBoundTypeConstants (HsNewTypeDecl _ _ name _ _ _) = S.singleton $ convertName name
    getBoundTypeConstants _                              = S.empty
