{-# Language GeneralizedNewtypeDeriving, TupleSections, MultiParamTypeClasses, FlexibleContexts, LambdaCase #-}

module Preprocessor.Renamer where

import Language.Haskell.Syntax
import Data.Hashable
import Data.Foldable
import Data.Default
import Data.Tuple
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
    deriving (Show)
instance Default RenamerState where
    def = RenamerState
            { variableCounter = 0
            , bindings = M.empty
            , reverseMapping = M.empty }

newtype Renamer a = Renamer (ExceptT String (State RenamerState) a)
    deriving (Applicative, Functor, Monad, MonadError String, MonadState RenamerState)
instance NameGenerator Renamer UniqueName where
    freshName = do
        counter <- gets variableCounter
        modify (\s -> s { variableCounter = counter + 1 })
        return (UniqueName $ Id $ "v" ++ show counter)

runRenamer :: (MonadError String m) => Renamer a -> (m a, RenamerState)
runRenamer (Renamer inner) = (liftEither x, s)
    where (x, s) = runState (runExceptT inner) def

type Rename a = a -> Renamer a

disjointUnion :: (MonadError String m, Ord a, Show a) => S.Set a -> S.Set a -> m (S.Set a)
disjointUnion s1 s2 = if S.null inter then return (S.union s1 s2) else throwError err
    where inter = S.intersection s1 s2
          err = printf "Duplicate binding names in same level: %s" (show $ S.toList inter)
disjointUnions :: (MonadError String m, Foldable f, Ord a, Show a) => f (S.Set a) -> m (S.Set a)
disjointUnions = foldlM disjointUnion S.empty

getUniqueScopedName :: Name -> Renamer UniqueName
getUniqueScopedName name = gets (M.lookup name . bindings) >>= \case
    Nothing -> throwError $ "Missing unique name for variable " ++ show name
    Just [] -> throwError $ "Variable out of scope " ++ show name
    Just (newname:_) -> return newname

bindForScope :: S.Set Name -> Renamer a -> Renamer a
bindForScope names action = do
    mapping <- M.fromList <$> mapM (\name -> (name,) <$> freshName) (S.toList names)
    -- Add new bindings to scope
    modify (\s -> s { bindings = M.unionWith (++) (M.map pure mapping) (bindings s) })
    -- Add reverse mappings
    modify (\s -> s { reverseMapping = M.union (reverseMapping s) (M.fromList $ map swap $ M.toList mapping) })
    -- Run the given action in the new nested scope
    result <- action
    -- Remove the names from the environment
    forM_ names $ \name -> do
        bindings' <- gets bindings
        case M.lookup name bindings' of
            Nothing -> throwError $ "Variable " ++ show name ++ " is not defined."
            Just [] -> throwError $ "Variable " ++ show name ++ " is not in scope."
            Just (_:bs) -> modify (\s -> s { bindings = M.insert name bs bindings' }) -- Pop the first binding
    return result

-- |The `getXBoundNames` functions should return the "binding" names of an `X`: eg. for a pattern binding, we want to
-- return all the variables in the pattern but not any in eg. a `where` clause attached to the binding. For a function
-- binding, we want to return the function name but not the variables bound by the argument patterns or any attached
-- `where` clauses.

-- |Return all variable names bound in this pattern (recursively)
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

-- |Get all variable names bound in this list of patterns
getPatsBoundNames :: MonadError String m => [HsPat] -> m (S.Set Name)
getPatsBoundNames ps = disjointUnions =<< mapM getPatBoundNames ps

-- |Get all variable names top-level bound by this declaration, **not recursively**. Eg. for the pattern binding:
-- > x = y
-- >   where y = 5
-- we return `x` but not `y`, as `x` is visible to horizontally defined bindings, whereas y is not.
getDeclBoundNames :: MonadError String m => HsDecl -> m (S.Set Name)
getDeclBoundNames (HsPatBind _ pat _ _) = getPatBoundNames pat
getDeclBoundNames (HsFunBind matches) = do
    let names = map (\(HsMatch _ name _ _ _) -> toId name) matches
        funName = head names
        allNamesMatch = all (== funName) names
    if allNamesMatch then return $ S.singleton (Name funName) else throwError "Mismatched function names"
getDeclBoundNames _ = throwError "Declaration not supported"

getDeclsBoundNames :: MonadError String m => [HsDecl] -> m (S.Set Name)
getDeclsBoundNames ds = disjointUnions =<< mapM getDeclBoundNames ds


-- |The `renameX` functions handle replacing variables in the syntax tree. They correctly handle horizontally-referenced
-- variables, like top-level definitions and functions defined in the same `let` expressions.

renameModule :: Rename HsModule
renameModule (HsModule a b c d e) = HsModule a b c d <$> renameDeclGroup e

rename :: Rename HsName
rename (HsIdent name) = getUniqueScopedName (Name $ Id name) >>= \(UniqueName (Id name')) -> return (HsIdent name')
rename (HsSymbol name) = getUniqueScopedName (Name $ Id name) >>= \(UniqueName (Id name')) -> return (HsSymbol name')
renameQ :: Rename HsQName
renameQ (Qual m n) = Qual m <$> rename n
renameQ (UnQual n) = UnQual <$> rename n
renameQ (Special _) = throwError "Special forms not supported"

-- |Rename a horizontally-grouped list of declarations, like in:
-- > x = y + 1
-- > y = x + 1
-- where each binding needs to be aware of the others.
renameDeclGroup :: Rename [HsDecl]
renameDeclGroup ds = fst <$> renameDeclGroupWith ds (pure ())
-- |Rename a horizontally-grouped list of declarations together with an auxiliary action
renameDeclGroupWith :: [HsDecl] -> Renamer a -> Renamer ([HsDecl], a)
renameDeclGroupWith decls action = do
    boundVars <- getDeclsBoundNames decls
    bindForScope boundVars ((,) <$> mapM renameDecl decls <*> action)

-- |Rename variables in a single declaration: here we take into account the nesting scope of a "where" clause
renameDecl :: Rename HsDecl
renameDecl (HsPatBind loc pat rhs decls) = HsPatBind loc <$> renamePat pat <*> renameRhs rhs <*> renameDeclGroup decls
renameDecl (HsFunBind matches) = HsFunBind <$> mapM renameMatch matches
renameDecl _ = throwError "Declaration not supported"

renameMatch :: Rename HsMatch
renameMatch (HsMatch loc funName pats rhs decls) = do
    argVars <- getPatsBoundNames pats
    whereVars <- getDeclsBoundNames decls
    boundVars <- disjointUnion argVars whereVars
    bindForScope boundVars (HsMatch loc <$> rename funName <*> renamePats pats <*> renameRhs rhs <*> mapM renameDecl decls)

renameRhs :: Rename HsRhs
renameRhs (HsUnGuardedRhs e) = HsUnGuardedRhs <$> renameExp e
renameRhs (HsGuardedRhss _) = throwError "Guarded RHS's not supported"

renamePat :: Rename HsPat
renamePat (HsPVar n) = HsPVar <$> rename n
renamePat l@(HsPLit _) = return l
renamePat (HsPNeg p) = HsPNeg <$> renamePat p
renamePat (HsPInfixApp p1 n p2) = HsPInfixApp <$> renamePat p1 <*> renameQ n <*> renamePat p2
renamePat (HsPApp n ps) = HsPApp <$> renameQ n <*> renamePats ps
renamePat (HsPTuple ps) = HsPTuple <$> renamePats ps
renamePat (HsPList ps) = HsPList <$> renamePats ps
renamePat (HsPParen p) = HsPParen <$> renamePat p
renamePat (HsPRec _ _) = throwError "Record fields not supported"
renamePat (HsPAsPat n p) = HsPAsPat <$> rename n <*> renamePat p
renamePat HsPWildCard = return HsPWildCard
renamePat (HsPIrrPat p) = HsPIrrPat <$> renamePat p
renamePats :: Rename [HsPat]
renamePats = mapM renamePat

renameQOp :: Rename HsQOp
renameQOp (HsQVarOp n) = HsQVarOp <$> renameQ n
renameQOp (HsQConOp n) = HsQConOp <$> renameQ n

renameExp :: Rename HsExp
renameExp (HsVar n) = HsVar <$> renameQ n
renameExp (HsCon n) = HsCon <$> renameQ n
renameExp l@(HsLit _) = return l
renameExp (HsInfixApp e1 op e2) = HsInfixApp <$> renameExp e1 <*> renameQOp op <*> renameExp e2
renameExp (HsApp e1 e2) = HsApp <$> renameExp e1 <*> renameExp e2
renameExp (HsNegApp e) = HsNegApp <$> renameExp e
renameExp (HsLambda l ps e) = do
    names <- getPatsBoundNames ps
    bindForScope names (HsLambda l <$> renamePats ps <*> renameExp e)
renameExp (HsLet decls e) = uncurry HsLet <$> renameDeclGroupWith decls (renameExp e)
renameExp (HsIf e1 e2 e3) = HsIf <$> renameExp e1 <*> renameExp e2 <*> renameExp e3
renameExp (HsCase _ _) = throwError "Case expression not supported"
renameExp (HsDo _) = throwError "Do expression not supported"
renameExp (HsTuple es) = HsTuple <$> renameExps es
renameExp (HsList es) = HsList <$> renameExps es
renameExp (HsParen e) = HsParen <$> renameExp e
renameExp _ = throwError "Renaming expression not supported"
renameExps:: Rename [HsExp]
renameExps = mapM renameExp