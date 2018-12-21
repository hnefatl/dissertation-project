{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts #-}

module Backend.Deoverload where

import Language.Haskell.Syntax
import Text.Printf
import Data.Default
import Data.Foldable
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Names
import NameGenerator
import Typechecker.Types

newtype Deoverload a = Deoverload (ExceptT String (StateT DeoverloadState NameGenerator) a)
    deriving (Functor, Applicative, Monad, MonadState DeoverloadState, MonadError String, MonadNameGenerator)

data DeoverloadState = DeoverloadState
    { dictionaries :: M.Map TypePredicate VariableName
    , types :: M.Map VariableName QuantifiedType }
    deriving (Eq, Show)
instance Default DeoverloadState where
    def = DeoverloadState
        { dictionaries = M.empty
        , types = M.empty }

runDeoverload :: Deoverload a -> NameGenerator (Except String a, DeoverloadState)
runDeoverload (Deoverload inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalDeoverload :: Deoverload a -> ExceptT String NameGenerator a
evalDeoverload (Deoverload inner) = do
    x <- lift $ evalStateT (runExceptT inner) def
    liftEither x


addTypes :: M.Map VariableName QuantifiedType -> Deoverload ()
addTypes ts = modify (\s -> s { types = M.union ts (types s) })
getType :: VariableName -> Deoverload (Maybe QuantifiedType)
getType name = gets (M.lookup name . types)

addDictionaries :: M.Map TypePredicate VariableName -> Deoverload ()
addDictionaries dicts = do
    existingDicts <- gets dictionaries
    let intersection = M.intersection dicts existingDicts
    unless (M.null intersection) $ throwError ("Clashing dictionary types: " ++ show intersection)
    modify (\s -> s { dictionaries = M.union existingDicts dicts })
addScopedDictionaries :: M.Map TypePredicate VariableName -> Deoverload a -> Deoverload a
addScopedDictionaries dicts action = do
    -- Add dictionaries, run the action, remove them
    addDictionaries dicts
    result <- action
    modify (\s -> s { dictionaries = M.difference (dictionaries s) dicts })
    return result
getDictionary :: TypePredicate -> Deoverload VariableName
getDictionary p = gets (M.lookup p . dictionaries) >>= \case
    Nothing -> throwError $ printf "Dictionary %s not found in environment" (show p)
    Just v -> return v
getDictionaryExp :: TypePredicate -> Deoverload HsExp
getDictionaryExp p = do
    VariableName name <- getDictionary p
    return $ HsVar $ UnQual (HsIdent name)

makeDictName :: TypePredicate -> Deoverload VariableName
makeDictName (IsInstance (TypeConstantName cl) t) = do
    TypeVariableName suffix <- case t of
        TypeVar (TypeVariable tvn _) -> return tvn
        TypeConstant{} -> freshTypeVarName
    return $ VariableName $ "d" ++ cl ++ suffix


-- |Given a pattern and access to a list of bindings, find what type was assigned to the pattern
patType :: HsPat -> Deoverload QuantifiedType
patType (HsPVar name) = getType (convertName name) >>= \case
    Nothing -> throwError $ printf "Variable %s not found in environment" (convertName name :: String)
    Just qt -> return qt
patType (HsPLit l) = litType l
patType (HsPApp con ps) = mergeQuantifiedTypes makeDataType <$> mapM patType ps
    where makeDataType = TypeConstant (convertName con) []
patType (HsPTuple ps) = mergeQuantifiedTypes mkTuple <$> mapM patType ps
    where mkTuple = TypeConstant (TypeConstantName "(,)") []
patType (HsPList ps) = mergeQuantifiedTypes mkList <$> mapM patType ps
    where mkList = TypeConstant (TypeConstantName "[]") []
patType (HsPParen p) = patType p
patType (HsPAsPat _ p) = patType p
patType HsPWildCard = do
    v <- TypeVariable <$> freshTypeVarName <*> pure KindStar
    return $ Quantified (S.singleton v) $ Qualified S.empty (TypeVar v)
patType _ = throwError "Unsupported pattern in pattern type reconstruction in deoverloader"

litType :: HsLiteral -> Deoverload QuantifiedType
litType (HsChar _) = return $ Quantified S.empty $ Qualified S.empty typeChar
litType (HsString _) = return$ Quantified S.empty $ Qualified S.empty typeString
litType (HsInt _) = do
    v <- TypeVariable <$> freshTypeVarName <*> pure KindStar
    let vt = TypeVar v
        num = TypeConstantName "Num"
    return $ Quantified (S.singleton v) $ Qualified (S.singleton $ IsInstance num vt) vt
litType (HsFrac _) = do
    v <- TypeVariable <$> freshTypeVarName <*> pure KindStar
    let vt = TypeVar v
        fractional = TypeConstantName "Fractional"
    return $ Quantified (S.singleton v) $ Qualified (S.singleton $ IsInstance fractional vt) vt
litType _ = throwError "Primitive types not supported in the deoverloader"

expType :: HsExp -> Deoverload QuantifiedType
expType (HsVar name) = getType (convertName name) >>= \case
    Nothing -> throwError $ "No type for name " ++ convertName name
    Just qt -> return qt
expType (HsCon name) = expType (HsVar name)
expType (HsLit l) = litType l
expType (HsInfixApp e1 op e2) = do
    let f = HsVar $ case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    expType (HsApp (HsApp f e1) e2)
expType (HsApp f e) = do
    fType <- expType f
    eType <- expType e
    throwError "Work this out on a whiteboard..."
expType (HsLambda _ ps e) = do
    pTypes <- mapM patType ps
    throwError "Work out"
expType (HsLet _ e) = expType e -- We can ignore the bindings as their types are already available
expType (HsIf _ e _) = expType e -- We passed typechecking, so we can just find the type of one branch
expType (HsTuple es) = mergeQuantifiedTypes makeTuple <$> mapM expType es
expType (HsList (e:_)) = do
    Quantified quants (Qualified quals t) <- expType e
    return $ Quantified quants $ Qualified quals $ makeList t
expType (HsParen e) = expType e
expType _ = throwError "Unsupported expression in expression type reconstruction in deoverloader"


-- TODO(kc506): Dependency order: we need to process class/data/instance declarations before function definitions.
-- Can wait until we properly support data declarations, as until then we're injecting the class/instance defns manually
deoverloadModule :: HsModule -> Deoverload HsModule
deoverloadModule (HsModule a b c d decls) = HsModule a b c d <$> deoverloadDecls decls

deoverloadDecls :: [HsDecl] -> Deoverload [HsDecl]
deoverloadDecls = mapM deoverloadDecl

deoverloadDecl :: HsDecl -> Deoverload HsDecl
deoverloadDecl (HsPatBind a pat rhs ds) = do
    -- Replace each constraint with a lambda for a dictionary
    Quantified _ (Qualified quals _) <- patType pat
    let constraints = S.toAscList quals
    args <- mapM makeDictName constraints
    let dictArgs = M.fromList $ zip constraints args
        -- Wrap an expression in a lambda that takes the dictionary arguments
        addArgs = HsLambda a [ HsPVar (HsIdent arg) | VariableName arg <- args ]
    rhs' <- addScopedDictionaries dictArgs (deoverloadRhs rhs)
    let resultRhs = case rhs' of
            HsUnGuardedRhs e -> HsUnGuardedRhs (addArgs e)
            HsGuardedRhss _ -> error "Guarded rhss not supported in deoverloader"
    return $ HsPatBind a pat resultRhs ds
deoverloadDecl _ = throwError "Unsupported declaration in deoverloader"

deoverloadRhs :: HsRhs -> Deoverload HsRhs
deoverloadRhs (HsUnGuardedRhs e) = HsUnGuardedRhs <$> deoverloadExp e
deoverloadRhs _ = throwError "Unsupported RHS in deoverloader"

deoverloadExp :: HsExp -> Deoverload HsExp
deoverloadExp (HsVar name) = getType (convertName name) >>= \case
    Nothing -> return (HsVar name)
    Just (Quantified _ (Qualified quals _)) -> foldl' HsApp (HsVar name) <$> mapM getDictionaryExp (S.toAscList quals)
    -- URGENT: Need to unify the simply type of the function with the argument types to get a substitution we can apply
    -- to the rest of the type... need to turn "Num a" into "Num t4".
deoverloadExp (HsCon name) = deoverloadExp (HsVar name)
deoverloadExp l@(HsLit _) = return l
deoverloadExp (HsApp f e) = HsApp <$> deoverloadExp f <*> deoverloadExp e
deoverloadExp (HsInfixApp e1 op e2) = do
    let fName = case op of
            HsQVarOp name -> name
            HsQConOp name -> name
    -- Convert an infix application into two normal applications
    fExp <- deoverloadExp (HsVar fName)
    e1Exp <- deoverloadExp e1
    e2Exp <- deoverloadExp e2
    return $ HsApp (HsApp fExp e1Exp) e2Exp
deoverloadExp (HsLambda a pats e) = HsLambda a pats <$> deoverloadExp e
deoverloadExp (HsIf c e1 e2) = HsIf <$> deoverloadExp c <*> deoverloadExp e1 <*> deoverloadExp e2
deoverloadExp (HsTuple es) = HsTuple <$> mapM deoverloadExp es
deoverloadExp (HsList es) = HsList <$> mapM deoverloadExp es
deoverloadExp (HsParen e) = HsParen <$> deoverloadExp e
deoverloadExp e = throwError $ "Unsupported expression in deoverloader: " ++ show e