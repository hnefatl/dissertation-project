{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

module Preprocessor.Renamer where

import           BasicPrelude                hiding (head)
import           Control.Monad.Except        (Except, ExceptT, MonadError, liftEither, runExceptT, throwError)
import           Control.Monad.State.Strict  (MonadState, StateT, evalStateT, gets, modify, runStateT)
import           Data.Default                (Default, def)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import           Data.Text                   (unpack)
import           Data.Tuple                  ()
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)
import           TextShow.Instances          ()

import           AlphaEq                     (stripModuleParens)
import           ExtraDefs                   (synPrint)
import           Logger                      (LoggerT, MonadLogger, writeLog)
import           NameGenerator
import           Names
import           Preprocessor.ContainedNames
import           Tuples                      (makeTupleName)

data RenamerState = RenamerState
    { -- Mappings from a variable name to a stack of unique names. The stack is to facilitate nesting.
      variableBindings           :: M.Map VariableName [UniqueVariableName]
      -- A reverse mapping from unique names to their original variable name: useful for printing error messages.
    , variableReverseMapping     :: M.Map UniqueVariableName VariableName
    -- Analogous to the above but for type variables
    , typeVariableBindings       :: M.Map TypeVariableName [UniqueTypeVariableName]
    , typeVariableReverseMapping :: M.Map UniqueTypeVariableName TypeVariableName }
    deriving (Eq, Show)
instance TextShow RenamerState where
    showb = fromString . show
instance Default RenamerState where
    def = RenamerState
            { variableBindings = M.empty
            , variableReverseMapping = M.empty
            , typeVariableBindings = M.empty
            , typeVariableReverseMapping = M.empty }

newtype Renamer a = Renamer (ExceptT Text (StateT RenamerState (LoggerT NameGenerator)) a)
    deriving (Applicative, Functor, Monad, MonadError Text, MonadState RenamerState, MonadLogger, MonadNameGenerator)

runRenamer :: Renamer a -> LoggerT NameGenerator (Except Text a, RenamerState)
runRenamer (Renamer inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalRenamer :: Renamer a -> ExceptT Text (LoggerT NameGenerator) a
evalRenamer (Renamer inner) = do
    x <- lift $ evalStateT (runExceptT inner) def
    liftEither x

getUniqueScopedVariableName :: VariableName -> Renamer UniqueVariableName
getUniqueScopedVariableName name = gets (M.lookup name . variableBindings) >>= \case
    Nothing -> throwError $ "Missing unique name for variable " <> showt name
    Just [] -> throwError $ "Variable out of scope " <> showt name
    Just (newname:_) -> return newname
getUniqueScopedTypeVariableName :: TypeVariableName -> Renamer UniqueTypeVariableName
getUniqueScopedTypeVariableName name = gets (M.lookup name . typeVariableBindings) >>= \case
    Nothing -> throwError $ "Missing unique name for type variable " <> showt name
    Just [] -> throwError $ "Type variable out of scope " <> showt name
    Just (newname:_) -> return newname

bindVariableForScope :: S.Set VariableName -> Renamer a -> Renamer a
bindVariableForScope names action = do
    mapping <- M.fromList <$> mapM (\name -> (name,) <$> freshUniqueVarName) (S.toList names)
    -- Add new bindings to scope
    modify (\s -> s { variableBindings = M.unionWith (<>) (M.map pure mapping) (variableBindings s) })
    writeLog $ "Entered new scope: bindings = " <> showt mapping
    -- Add reverse mappings
    let reverseMapping = M.fromList $ map swap $ M.toList mapping
    modify (\s -> s { variableReverseMapping = M.union (variableReverseMapping s) reverseMapping })
    -- Run the given action in the new nested scope
    result <- action
    -- Remove the names from the environment
    forM_ names $ \name -> do
        bindings' <- gets variableBindings
        case M.lookup name bindings' of
            Nothing     -> throwError $ "Variable " <> showt name <> " is not defined."
            Just []     -> throwError $ "Variable " <> showt name <> " is not in scope."
            Just (_:bs) -> modify (\s -> s { variableBindings = M.insert name bs bindings' }) -- Pop the first binding
    return result

-- We need to duplicate the code :( We can't parametrise on record fields like `typeVariableBindings`. Could use
-- lenses...
bindTypeVariableForScope :: S.Set TypeVariableName -> Renamer a -> Renamer a
bindTypeVariableForScope names action = do
    mapping <- M.fromList <$> mapM (\name -> (name,) <$> freshUniqueTypeVarName) (S.toList names)
    oldBindings <- gets typeVariableBindings
    modify (\s -> s { typeVariableBindings = M.unionWith (<>) (M.map pure mapping) oldBindings })
    let reverseMapping = M.fromList $ map swap $ M.toList mapping
    modify (\s -> s { typeVariableReverseMapping = M.union (typeVariableReverseMapping s) reverseMapping })
    result <- action
    modify (\s -> s { typeVariableBindings = oldBindings })
    return result


-- |The `renameX` functions handle replacing variables in the syntax tree. They correctly handle horizontally-referenced
-- variables, like top-level definitions and functions defined in the same `let` expressions.

class Renameable a where
    rename :: a -> Renamer a
renames :: (Renameable a, Traversable f) => f a -> Renamer (f a)
renames = mapM rename

renameIsolated :: (Renameable a, HasBoundVariables a) => a -> ExceptT Text (LoggerT NameGenerator) (a, M.Map VariableName VariableName)
renameIsolated x = do
    bound <- getBoundVariables x
    evalRenamer $ bindVariableForScope bound $ do
        renamed <- rename x
        mapping <- fmap (\[UniqueVariableName ren] -> VariableName ren) <$> gets variableBindings
        return (renamed, mapping)

renameVariable :: HsName -> Renamer HsName
renameVariable n@HsIdent{} = HsIdent . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameVariable n           = HsSymbol . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameTypeVariable :: HsName -> Renamer HsName
renameTypeVariable n@HsIdent{} = HsIdent . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)
renameTypeVariable n           = HsSymbol . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)

-- Returns the renamed module along with the top-level renaming and reverse variable renaming mapping
renameModule :: HsModule -> Renamer (HsModule, M.Map VariableName VariableName, M.Map VariableName VariableName)
renameModule (HsModule a b c d decls) = do
    writeLog "-----------"
    writeLog "- Renamer -"
    writeLog "-----------"
    boundVars <- getBoundVariables decls
    bindVariableForScope boundVars $ do
        topLevelMappings <- gets variableBindings
        decls' <- renames decls
        reverseMapping <- gets variableReverseMapping
        let reverseRenames = M.mapKeys (\(UniqueVariableName n) -> VariableName n) reverseMapping
        topLevelRenames <- forM topLevelMappings $ \case
            [UniqueVariableName n] -> return $ VariableName n
            rs -> throwError $ unlines ["Got illegal top level renaming:", showt rs]
        writeLog "Renames"
        forM_ (M.toList reverseRenames) $ \(r, v) -> writeLog $ showt r <> ": " <> showt v
        return (stripModuleParens $ HsModule a b c d decls', topLevelRenames, reverseRenames)

instance Renameable HsQName where
    rename (Qual m n)  = Qual m <$> renameVariable n
    rename (UnQual n)  = UnQual <$> renameVariable n
    rename n@Special{} = UnQual <$> renameVariable (HsSymbol $ unpack $ convertName n)

instance Renameable HsQOp where
    rename (HsQVarOp n) = HsQVarOp <$> rename n
    rename (HsQConOp c) = HsQConOp <$> rename c

-- |Rename a horizontally-grouped list of declarations, like in:
-- > x = y + 1
-- > y = x + 1
-- where each binding needs to be aware of the others.
instance Renameable [HsDecl] where
    rename ds = fst <$> renameDeclGroupWith ds (pure ())
-- |Rename a horizontally-grouped list of declarations together with an auxiliary action that's in scope of the
-- renamings
renameDeclGroupWith :: [HsDecl] -> Renamer a -> Renamer ([HsDecl], a)
renameDeclGroupWith decls action = do
    boundVars <- getBoundVariables decls
    bindVariableForScope boundVars ((,) <$> renames decls <*> action)

-- |Rename variables in a single declaration: here we take into account the nesting scope of a "where" clause
instance Renameable HsDecl where
    rename (HsPatBind loc pat rhs decls) = HsPatBind loc <$> rename pat <*> rename rhs <*> rename decls
    rename (HsFunBind matches)           = HsFunBind <$> renames matches
    rename (HsTypeSig loc names t)       = HsTypeSig loc <$> mapM renameVariable names <*> rename t
    rename (HsClassDecl loc ctx name args decls) =
        -- Bind the argument variable at an outer scope
        bindTypeVariableForScope (S.fromList $ map convertName args) $ do
            decls' <- forM decls $ \case
                HsTypeSig loc' names t ->
                    HsTypeSig loc' <$> mapM renameVariable names <*> renameQualTypeWithExistingScope t
                _ -> throwError "Non-HsTypeSig in typeclass"
            HsClassDecl loc <$> renames ctx <*> pure name <*> mapM renameTypeVariable args <*> pure decls'
    rename (HsDataDecl loc ctx name args decls derivings) =
        bindTypeVariableForScope (S.fromList $ map convertName args) $
            HsDataDecl loc ctx name <$> mapM renameTypeVariable args <*> renames decls <*> pure derivings
    rename (HsInstDecl loc ctx cname ts ds) = HsInstDecl loc <$> renames ctx <*> pure cname <*> renames ts <*> renames ds
    rename d                             = throwError $ unlines ["Declaration not supported:", synPrint d]

instance Renameable HsConDecl where
    rename (HsConDecl loc name args) = HsConDecl loc <$> renameVariable name <*> renames args
    rename HsRecDecl{}               = throwError "Record data declarations not supported in renamer"

instance Renameable HsBangType where
    rename (HsBangedTy t)   = HsBangedTy <$> rename t
    rename (HsUnBangedTy t) = HsUnBangedTy <$> rename t

instance Renameable HsMatch where
    rename (HsMatch loc funName pats rhs decls) = do
        argVars <- getBoundVariablesAndConflicts pats
        whereVars <- getBoundVariablesAndConflicts decls
        boundVars <- M.keysSet <$> disjointUnion argVars whereVars
        let action = HsMatch loc <$> renameVariable funName <*> renames pats <*> rename rhs <*> rename decls
        bindVariableForScope boundVars action

instance Renameable HsRhs where
    rename (HsUnGuardedRhs e) = HsUnGuardedRhs <$> rename e
    rename (HsGuardedRhss _)  = throwError "Guarded RHS's not supported"

instance Renameable HsPat where
    rename (HsPVar n)            = HsPVar <$> renameVariable n
    rename l@HsPLit{}            = return l
    rename (HsPNeg p)            = HsPNeg <$> rename p
    rename (HsPInfixApp p1 n p2) = HsPInfixApp <$> rename p1 <*> rename n <*> rename p2
    rename (HsPApp con ps)       = HsPApp <$> rename con <*> renames ps
    rename (HsPTuple ps) = do
        let tupleCon = UnQual $ HsSymbol $ unpack $ makeTupleName $ length ps
        rename $ HsPApp tupleCon ps
    rename (HsPList ps) = do
        let listCon = UnQual $ HsSymbol "[]"
        rename $ HsPApp listCon ps
    rename (HsPParen p)          = HsPParen <$> rename p
    rename HsPRec{}              = throwError "Record fields not supported"
    rename (HsPAsPat n p)        = HsPAsPat <$> renameVariable n <*> rename p
    rename HsPWildCard           = return HsPWildCard
    rename (HsPIrrPat p)         = HsPIrrPat <$> rename p

instance Renameable HsExp where
    rename (HsVar n) = HsVar <$> rename n
    rename (HsCon c) = HsCon <$> rename c
    rename l@(HsLit _) = return l
    rename (HsInfixApp e1 op e2) = HsInfixApp <$> rename e1 <*> rename op <*> rename e2
    rename (HsApp e1 e2) = HsApp <$> rename e1 <*> rename e2
    rename (HsNegApp e) = HsNegApp <$> rename e
    rename (HsLambda l ps e) = do
        names <- getBoundVariables ps
        bindVariableForScope names (HsLambda l <$> renames ps <*> rename e)
    rename (HsLet decls e) = uncurry HsLet <$> renameDeclGroupWith decls (rename e)
    rename (HsIf e1 e2 e3) = HsIf <$> rename e1 <*> rename e2 <*> rename e3
    rename (HsCase head alts) = HsCase <$> rename head <*> renames alts
    rename (HsDo _) = throwError "Do expression not supported"
    -- We rename the builtin tuple/list syntax to an application of the appropriate constructor, as otherwise they
    -- remain list/tuple constructors later when the actual list/tuple constructors have been renamed to something else.
    rename (HsTuple es) = do
        let tupleCon = HsCon $ UnQual $ HsSymbol $ unpack $ makeTupleName $ length es
        rename $ foldl' HsApp tupleCon es
    rename (HsList es) = do
        let listCon = HsCon $ UnQual $ HsSymbol "[]"
        rename $ foldl' HsApp listCon es
    rename (HsParen e) = HsParen <$> rename e
    rename (HsExpTypeSig l e t) = HsExpTypeSig l <$> rename e <*> rename t
    rename _ = throwError "Renaming expression not supported"
instance Renameable HsAlt where
    rename (HsAlt loc pat alts wheres) = do
        names <- getBoundVariables pat
        bindVariableForScope names (HsAlt loc <$> rename pat <*> rename alts <*> renames wheres)
instance Renameable HsGuardedAlts where
    rename (HsUnGuardedAlt e) = HsUnGuardedAlt <$> rename e
    rename (HsGuardedAlts as) = HsGuardedAlts <$> renames as
instance Renameable HsGuardedAlt where
    rename (HsGuardedAlt loc cond e) = HsGuardedAlt loc <$> rename cond <*> rename e

renameQualTypeWithExistingScope :: HsQualType -> Renamer HsQualType
renameQualTypeWithExistingScope qt@(HsQualType quals t) = do
    existingBindings <- gets (M.keysSet . typeVariableBindings)
    let contained = getFreeTypeVariables qt
        newBindings = S.difference contained existingBindings
    tvbs <- gets typeVariableBindings
    writeLog $ unwords ["renameQualTypeWithExistingScope:", synPrint qt, showt tvbs, showt contained, showt newBindings]
    bindTypeVariableForScope newBindings $ HsQualType <$> renames quals <*> rename t

instance Renameable HsQualType where
    rename qt = bindTypeVariableForScope contained (renameQualTypeWithExistingScope qt)
        where contained = getFreeTypeVariables qt
instance Renameable HsAsst where
    rename (name, ts) = (name,) <$> renames ts
instance Renameable HsType where
    rename (HsTyFun t1 t2) = HsTyFun <$> rename t1 <*> rename t2
    rename (HsTyTuple ts)  = HsTyTuple <$> renames ts
    rename (HsTyApp t1 t2) = HsTyApp <$> rename t1 <*> rename t2
    rename (HsTyVar n)     = HsTyVar <$> renameTypeVariable n
    rename (HsTyCon c)     = return $ HsTyCon c
