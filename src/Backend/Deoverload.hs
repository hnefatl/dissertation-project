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
instance Default DeoverloadState where
    def = DeoverloadState
        { dictionaries = M.empty
        , types = M.empty }

runDeoverload :: MonadError String m => Deoverload a -> NameGenerator (m a, DeoverloadState)
runDeoverload (Deoverload inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalDeoverload :: MonadError String m => Deoverload a -> NameGenerator (m a)
evalDeoverload x = fst <$> runDeoverload x

getType :: VariableName -> Deoverload (Maybe QuantifiedType)
getType name = gets (M.lookup name . types)

getDictionary :: TypePredicate -> Deoverload VariableName
getDictionary p = gets (M.lookup p . dictionaries) >>= \case
    Nothing -> throwError $ printf "Dictionary %s not found in environment" (show p)
    Just v -> return v
getDictionaryExp :: TypePredicate -> Deoverload HsExp
getDictionaryExp p = do
    VariableName name <- getDictionary p
    return $ HsVar $ UnQual (HsIdent name)

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

makeDictName :: TypePredicate -> Deoverload VariableName
makeDictName (IsInstance (TypeConstantName cl) t) = do
    TypeVariableName suffix <- case t of
        TypeVar (TypeVariable tvn _) -> return tvn
        TypeConstant{} -> freshTypeVarName
    return $ VariableName $ "d" ++ cl ++ suffix


-- |Given a pattern and access to a list of bindings, find what type was assigned to the pattern
patternType :: HsPat -> Deoverload QuantifiedType 
patternType (HsPVar name) = getType (convertName name) >>= \case
    Nothing -> throwError $ printf "Variable %s not found in environment" (show name)
    Just qt -> return qt
patternType (HsPLit l) = litType l
patternType (HsPApp con ps) = mergeQuantifiedTypes makeDataType <$> mapM patternType ps
    where makeDataType = TypeConstant (convertName con) []
patternType (HsPTuple ps) = mergeQuantifiedTypes mkTuple <$> mapM patternType ps
    where mkTuple = TypeConstant (TypeConstantName "(,)") []
patternType (HsPList ps) = mergeQuantifiedTypes mkList <$> mapM patternType ps
    where mkList = TypeConstant (TypeConstantName "[]") []
patternType (HsPParen p) = patternType p
patternType (HsPAsPat _ p) = patternType p
patternType HsPWildCard = do
    v <- TypeVariable <$> freshTypeVarName <*> pure KindStar
    return $ Quantified (S.singleton v) $ Qualified S.empty (TypeVar v)
patternType _ = throwError "Unsupported pattern in deoverloader"

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

deoverloadModule :: HsModule -> Deoverload HsModule
deoverloadModule (HsModule a b c d decls) = HsModule a b c d <$> deoverloadDecls decls

deoverloadDecls :: [HsDecl] -> Deoverload [HsDecl]
deoverloadDecls = mapM deoverloadDecl

deoverloadDecl :: HsDecl -> Deoverload HsDecl
deoverloadDecl (HsPatBind a pat rhs ds) = do
    -- Replace each constraint with a lambda for a dictionary
    Quantified _ (Qualified quals _) <- patternType pat
    let constraints = S.toAscList quals
    args <- mapM makeDictName constraints
    let dictArgs = M.fromList $ zip constraints args
        -- Wrap an expression in a lambda that takes the dictionary arguments
        addArgs e = HsLambda a [ HsPVar (HsIdent arg) | VariableName arg <- args ] e
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
    Just (Quantified _ (Qualified quals _)) -> foldl' HsApp (HsVar name) <$> (mapM getDictionaryExp $ S.toAscList quals)
deoverloadExp (HsCon name) = deoverloadExp (HsVar name)
deoverloadExp l@(HsLit _) = return l
deoverloadExp (HsApp f e) = HsApp <$> deoverloadExp f <*> deoverloadExp e
deoverloadExp (HsLambda a pats e) = HsLambda a pats <$> deoverloadExp e
deoverloadExp (HsIf c e1 e2) = HsIf <$> deoverloadExp c <*> deoverloadExp e1 <*> deoverloadExp e2
deoverloadExp (HsTuple es) = HsTuple <$> mapM deoverloadExp es
deoverloadExp (HsList es) = HsList <$> mapM deoverloadExp es
deoverloadExp (HsParen e) = HsParen <$> deoverloadExp e
deoverloadExp _ = throwError "Unsupported expression in deoverloader"