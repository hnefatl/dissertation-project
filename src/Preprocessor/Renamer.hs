{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

module Preprocessor.Renamer where

import           BasicPrelude
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

import           NameGenerator
import           Names
import           Preprocessor.ContainedNames
import           Typechecker.Hardcoded

data RenamerState = RenamerState
      -- Used to generate unique variable names
    { variableCounter            :: Int
      -- Mappings from a variable name to a stack of unique names. The stack is to facilitate nesting.
    , variableBindings           :: M.Map VariableName [UniqueVariableName]
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
            { variableCounter = 0
            , variableBindings = M.empty
            , variableReverseMapping = M.empty
            , typeVariableBindings = M.empty
            , typeVariableReverseMapping = M.empty }

renameModule :: HsModule -> ExceptT Text NameGenerator HsModule
renameModule = evalRenamer . rename

newtype Renamer a = Renamer (ExceptT Text (StateT RenamerState NameGenerator) a)
    deriving (Applicative, Functor, Monad, MonadError Text, MonadState RenamerState, MonadNameGenerator)

runRenamer :: Renamer a -> NameGenerator (Except Text a, RenamerState)
runRenamer (Renamer inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalRenamer :: Renamer a -> ExceptT Text NameGenerator a
evalRenamer (Renamer inner) = do
    x <- lift $ evalStateT (runExceptT inner) def
    liftEither x

getUniqueScopedName :: (Ord n, TextShow n) => (RenamerState -> M.Map n [un]) -> n -> Renamer un
getUniqueScopedName f name = gets (M.lookup name . f) >>= \case
    Nothing -> throwError $ "Missing unique name for variable " <> showt name
    Just [] -> throwError $ "Variable out of scope " <> showt name
    Just (newname:_) -> return newname
getUniqueScopedVariableName :: VariableName -> Renamer UniqueVariableName
getUniqueScopedVariableName = getUniqueScopedName variableBindings
getUniqueScopedTypeVariableName :: TypeVariableName -> Renamer UniqueTypeVariableName
getUniqueScopedTypeVariableName = getUniqueScopedName typeVariableBindings

bindVariableForScope :: S.Set VariableName -> Renamer a -> Renamer a
bindVariableForScope names action = do
    mapping <- M.fromList <$> mapM (\name -> (name,) <$> freshUniqueVarName) (S.toList names)
    -- Add new bindings to scope
    modify (\s -> s { variableBindings = M.unionWith (++) (M.map pure mapping) (variableBindings s) })
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
    modify (\s -> s { typeVariableBindings = M.unionWith (++) (M.map pure mapping) (typeVariableBindings s) })
    let reverseMapping = M.fromList $ map swap $ M.toList mapping
    modify (\s -> s { typeVariableReverseMapping = M.union (typeVariableReverseMapping s) reverseMapping })
    result <- action
    forM_ names $ \name -> do
        bindings' <- gets typeVariableBindings
        case M.lookup name bindings' of
            Nothing     -> throwError $ "Type Variable " <> showt name <> " is not defined."
            Just []     -> throwError $ "Type Variable " <> showt name <> " is not in scope."
            Just (_:bs) -> modify (\s -> s { typeVariableBindings = M.insert name bs bindings' })
    return result


-- |The `renameX` functions handle replacing variables in the syntax tree. They correctly handle horizontally-referenced
-- variables, like top-level definitions and functions defined in the same `let` expressions.

class Renameable a where
    rename :: a -> Renamer a
renames :: (Renameable a, Traversable f) => f a -> Renamer (f a)
renames = mapM rename

renameVariable :: HsName -> Renamer HsName
renameVariable n@(HsIdent _)  = HsIdent . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameVariable n@(HsSymbol _) = HsSymbol . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameTypeVariable :: HsName -> Renamer HsName
renameTypeVariable n@(HsIdent _) = HsIdent . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)
renameTypeVariable n@(HsSymbol _) = HsSymbol . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)

instance Renameable HsModule where
    rename (HsModule a b c d e) = do
        -- TODO(kc506): Remove when we get rid of hardcoded variables
        let fs = M.fromSet (\(VariableName n) -> [UniqueVariableName n]) (M.keysSet builtinFunctions)
        modify (\s -> s { variableBindings = M.union fs (variableBindings s) })
        HsModule a b c d <$> rename e
instance Renameable HsQName where
    rename (Qual m n)  = Qual m <$> renameVariable n
    rename (UnQual n)  = UnQual <$> renameVariable n
    rename (Special _) = throwError "Special forms not supported"

instance Renameable HsQOp where
    rename (HsQVarOp n) = HsQVarOp <$> rename n
    rename (HsQConOp c) = return $ HsQConOp c

-- |Rename a horizontally-grouped list of declarations, like in:
-- > x = y + 1
-- > y = x + 1
-- where each binding needs to be aware of the others.
instance Renameable [HsDecl] where
    rename ds = fst <$> renameDeclGroupWith ds (pure ())
-- |Rename a horizontally-grouped list of declarations together with an auxiliary action
renameDeclGroupWith :: [HsDecl] -> Renamer a -> Renamer ([HsDecl], a)
renameDeclGroupWith decls action = do
    boundVars <- getBoundVariables decls
    bindVariableForScope boundVars ((,) <$> renames decls <*> action)

-- |Rename variables in a single declaration: here we take into account the nesting scope of a "where" clause
instance Renameable HsDecl where
    rename (HsPatBind loc pat rhs decls) = HsPatBind loc <$> rename pat <*> rename rhs <*> rename decls
    rename (HsFunBind matches)           = HsFunBind <$> renames matches
    rename _                             = throwError "Declaration not supported"

instance Renameable HsMatch where
    rename (HsMatch loc funName pats rhs decls) = do
        argVars <- getBoundVariables pats
        whereVars <- getBoundVariables decls
        boundVars <- disjointUnion argVars whereVars
        let action = HsMatch loc <$> renameVariable funName <*> renames pats <*> rename rhs <*> rename decls
        bindVariableForScope boundVars action

instance Renameable HsRhs where
    rename (HsUnGuardedRhs e) = HsUnGuardedRhs <$> rename e
    rename (HsGuardedRhss _)  = throwError "Guarded RHS's not supported"

instance Renameable HsPat where
    rename (HsPVar n)            = HsPVar <$> renameVariable n
    rename l@(HsPLit _)          = return l
    rename (HsPNeg p)            = HsPNeg <$> rename p
    rename (HsPInfixApp p1 n p2) = HsPInfixApp <$> rename p1 <*> rename n <*> rename p2
    rename (HsPApp con ps)       = HsPApp con <$> renames ps
    rename (HsPTuple ps)         = HsPTuple <$> renames ps
    rename (HsPList ps)          = HsPList <$> renames ps
    rename (HsPParen p)          = HsPParen <$> rename p
    rename (HsPRec _ _)          = throwError "Record fields not supported"
    rename (HsPAsPat n p)        = HsPAsPat <$> renameVariable n <*> rename p
    rename HsPWildCard           = return HsPWildCard
    rename (HsPIrrPat p)         = HsPIrrPat <$> rename p

instance Renameable HsExp where
    rename (HsVar n) = HsVar <$> rename n
    rename (HsCon c) = return $ HsCon c
    rename l@(HsLit _) = return l
    rename (HsInfixApp e1 op e2) = HsInfixApp <$> rename e1 <*> rename op <*> rename e2
    rename (HsApp e1 e2) = HsApp <$> rename e1 <*> rename e2
    rename (HsNegApp e) = HsNegApp <$> rename e
    rename (HsLambda l ps e) = do
        names <- getBoundVariables ps
        bindVariableForScope names (HsLambda l <$> renames ps <*> rename e)
    rename (HsLet decls e) = uncurry HsLet <$> renameDeclGroupWith decls (rename e)
    rename (HsIf e1 e2 e3) = HsIf <$> rename e1 <*> rename e2 <*> rename e3
    rename (HsCase _ _) = throwError "Case expression not supported"
    rename (HsDo _) = throwError "Do expression not supported"
    rename (HsTuple es) = HsTuple <$> renames es
    rename (HsList es) = HsList <$> renames es
    rename (HsParen e) = HsParen <$> rename e
    rename (HsExpTypeSig l e t) = HsExpTypeSig l <$> rename e <*> rename t
    rename _ = throwError "Renaming expression not supported"

instance Renameable HsQualType where
    rename qt@(HsQualType quals t) = bindTypeVariableForScope contained action
        where contained = getFreeTypeVariables qt
              action = HsQualType <$> renames quals <*> rename t
instance Renameable HsAsst where
    rename (name, ts) = (name,) <$> renames ts
instance Renameable HsType where
    rename (HsTyFun t1 t2) = HsTyFun <$> rename t1 <*> rename t2
    rename (HsTyTuple ts)  = HsTyTuple <$> renames ts
    rename (HsTyApp t1 t2) = HsTyApp <$> rename t1 <*> rename t2
    rename (HsTyVar n)     = HsTyVar <$> renameTypeVariable n
    rename (HsTyCon c)     = return $ HsTyCon c
