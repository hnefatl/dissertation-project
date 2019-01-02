{-# LANGUAGE FlexibleContexts           #-}
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

type Rename a = a -> Renamer a

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

renameModule :: Rename HsModule
renameModule (HsModule a b c d e) = HsModule a b c d <$> renameDeclGroup e

renameVariable :: Rename HsName
renameVariable n@(HsIdent _)  = HsIdent . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameVariable n@(HsSymbol _) = HsSymbol . unpack . convertName <$> getUniqueScopedVariableName (convertName n)
renameVariableQ :: Rename HsQName
renameVariableQ (Qual m n)  = Qual m <$> renameVariable n
renameVariableQ (UnQual n)  = UnQual <$> renameVariable n
renameVariableQ (Special _) = throwError "Special forms not supported"
renameTypeVariable :: Rename HsName
renameTypeVariable n@(HsIdent _) = HsIdent . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)
renameTypeVariable n@(HsSymbol _) = HsSymbol . unpack . convertName <$> getUniqueScopedTypeVariableName (convertName n)

renameQOp :: Rename HsQOp
renameQOp (HsQVarOp n) = HsQVarOp <$> renameVariableQ n
renameQOp (HsQConOp c) = return $ HsQConOp c

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
    bindVariableForScope boundVars ((,) <$> mapM renameDecl decls <*> action)

-- |Rename variables in a single declaration: here we take into account the nesting scope of a "where" clause
renameDecl :: Rename HsDecl
renameDecl (HsPatBind loc pat rhs decls) = HsPatBind loc <$> renamePat pat <*> renameRhs rhs <*> renameDeclGroup decls
renameDecl (HsFunBind matches)           = HsFunBind <$> mapM renameMatch matches
renameDecl _                             = throwError "Declaration not supported"

renameMatch :: Rename HsMatch
renameMatch (HsMatch loc funName pats rhs decls) = do
    argVars <- getPatsContainedNames pats
    whereVars <- getDeclsBoundNames decls
    boundVars <- disjointUnion argVars whereVars
    let action = HsMatch loc <$> renameVariable funName <*> renamePats pats <*> renameRhs rhs <*> mapM renameDecl decls
    bindVariableForScope boundVars action

renameRhs :: Rename HsRhs
renameRhs (HsUnGuardedRhs e) = HsUnGuardedRhs <$> renameExp e
renameRhs (HsGuardedRhss _)  = throwError "Guarded RHS's not supported"

renamePat :: Rename HsPat
renamePat (HsPVar n)            = HsPVar <$> renameVariable n
renamePat l@(HsPLit _)          = return l
renamePat (HsPNeg p)            = HsPNeg <$> renamePat p
renamePat (HsPInfixApp p1 n p2) = HsPInfixApp <$> renamePat p1 <*> renameVariableQ n <*> renamePat p2
renamePat (HsPApp con ps)       = HsPApp con <$> renamePats ps
renamePat (HsPTuple ps)         = HsPTuple <$> renamePats ps
renamePat (HsPList ps)          = HsPList <$> renamePats ps
renamePat (HsPParen p)          = HsPParen <$> renamePat p
renamePat (HsPRec _ _)          = throwError "Record fields not supported"
renamePat (HsPAsPat n p)        = HsPAsPat <$> renameVariable n <*> renamePat p
renamePat HsPWildCard           = return HsPWildCard
renamePat (HsPIrrPat p)         = HsPIrrPat <$> renamePat p
renamePats :: Rename [HsPat]
renamePats = mapM renamePat

renameExp :: Rename HsExp
renameExp (HsVar n) = HsVar <$> renameVariableQ n
renameExp (HsCon c) = return $ HsCon c
renameExp l@(HsLit _) = return l
renameExp (HsInfixApp e1 op e2) = HsInfixApp <$> renameExp e1 <*> renameQOp op <*> renameExp e2
renameExp (HsApp e1 e2) = HsApp <$> renameExp e1 <*> renameExp e2
renameExp (HsNegApp e) = HsNegApp <$> renameExp e
renameExp (HsLambda l ps e) = do
    names <- getPatsContainedNames ps
    bindVariableForScope names (HsLambda l <$> renamePats ps <*> renameExp e)
renameExp (HsLet decls e) = uncurry HsLet <$> renameDeclGroupWith decls (renameExp e)
renameExp (HsIf e1 e2 e3) = HsIf <$> renameExp e1 <*> renameExp e2 <*> renameExp e3
renameExp (HsCase _ _) = throwError "Case expression not supported"
renameExp (HsDo _) = throwError "Do expression not supported"
renameExp (HsTuple es) = HsTuple <$> renameExps es
renameExp (HsList es) = HsList <$> renameExps es
renameExp (HsParen e) = HsParen <$> renameExp e
renameExp (HsExpTypeSig l e t) = HsExpTypeSig l <$> renameExp e <*> renameQualType t
renameExp _ = throwError "Renaming expression not supported"
renameExps:: Rename [HsExp]
renameExps = mapM renameExp

renameQualType :: Rename HsQualType
renameQualType qt@(HsQualType quals t) = bindTypeVariableForScope contained action
    where contained = getQualTypeContainedNames qt
          action = HsQualType <$> renameAssts quals <*> renameType t
renameAsst :: Rename HsAsst
renameAsst (name, ts) = (name,) <$> mapM renameType ts
renameAssts :: Rename [HsAsst]
renameAssts = mapM renameAsst
renameType :: Rename HsType
renameType (HsTyFun t1 t2) = HsTyFun <$> renameType t1 <*> renameType t2
renameType (HsTyTuple ts)  = HsTyTuple <$> mapM renameType ts
renameType (HsTyApp t1 t2) = HsTyApp <$> renameType t1 <*> renameType t2
renameType (HsTyVar n)     = HsTyVar <$> renameTypeVariable n
renameType (HsTyCon c)     = return $ HsTyCon c
