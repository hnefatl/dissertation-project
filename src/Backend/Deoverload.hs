{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, TemplateHaskell #-}

module Backend.Deoverload where

import BasicPrelude hiding (exp)
import TextShow (showt)
import TextShow.TH (deriveTextShow)
import Language.Haskell.Syntax
import Data.Default (Default, def)
import Data.Foldable ()
import Data.Text (unpack)
import Control.Monad.State.Strict (MonadState, StateT, runStateT, evalStateT, modify, gets)
import Control.Monad.Except (MonadError, ExceptT, Except, runExceptT, throwError, liftEither)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

import ExtraDefs (prettyPrintT)
import Names (VariableName(..), TypeVariableName(..), convertName)
import NameGenerator (MonadNameGenerator, NameGenerator, freshTypeVarName)
import TextShowHsSrc ()
import Typechecker.Types
import Typechecker.Hardcoded (builtinKinds)
import Typechecker.Typeclasses (ClassEnvironment, entails)
import Typechecker.Substitution (applySub)
import Typechecker.Unifier (mgu)

newtype Deoverload a = Deoverload (ExceptT Text (StateT DeoverloadState NameGenerator) a)
    deriving (Functor, Applicative, Monad, MonadState DeoverloadState, MonadError Text, MonadNameGenerator)

data DeoverloadState = DeoverloadState
    { dictionaries :: M.Map TypePredicate VariableName
    , types :: M.Map VariableName QuantifiedType
    , kinds :: M.Map TypeVariableName Kind
    , classEnvironment :: ClassEnvironment
    , logs :: Seq.Seq Text }
    deriving (Eq)
deriveTextShow ''DeoverloadState
instance Default DeoverloadState where
    def = DeoverloadState
        { dictionaries = M.empty
        , types = M.empty
        , kinds = M.empty
        , classEnvironment = M.empty
        , logs = Seq.empty }

runDeoverload :: Deoverload a -> NameGenerator (Except Text a, DeoverloadState)
runDeoverload (Deoverload inner) = do
    (x, s) <- runStateT (runExceptT inner) def
    return (liftEither x, s)

evalDeoverload :: Deoverload a -> ExceptT Text NameGenerator a
evalDeoverload (Deoverload inner) = do
    x <- lift $ evalStateT (runExceptT inner) def
    liftEither x

writeLog :: Text -> Deoverload ()
writeLog l = modify (\s -> s { logs = logs s Seq.|> l })

addTypes :: M.Map VariableName QuantifiedType -> Deoverload ()
addTypes ts = modify (\s -> s { types = M.union ts (types s) })
tryGetType :: VariableName -> Deoverload (Maybe QuantifiedType)
tryGetType name = gets (M.lookup name . types)
getType :: VariableName -> Deoverload QuantifiedType
getType name = tryGetType name >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not found in environment"
    Just qt -> return qt

addKinds :: M.Map TypeVariableName Kind -> Deoverload ()
addKinds ks = modify (\s -> s { kinds = M.union ks (kinds s) })
getKinds :: Deoverload (M.Map TypeVariableName Kind)
getKinds = gets kinds
getKind :: TypeVariableName -> Deoverload (Maybe Kind)
getKind name = M.lookup name <$> getKinds

addClassEnvironment :: ClassEnvironment -> Deoverload ()
addClassEnvironment ce = modify (\s -> s { classEnvironment = M.union ce (classEnvironment s) } )
getClassEnvironment :: Deoverload ClassEnvironment
getClassEnvironment = gets classEnvironment

addDictionaries :: M.Map TypePredicate VariableName -> Deoverload ()
addDictionaries dicts = do
    existingDicts <- gets dictionaries
    let intersection = M.intersection dicts existingDicts
    unless (M.null intersection) $ throwError $ "Clashing dictionary types: " <> showt intersection
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
    Nothing -> throwError $ "Dictionary " <> showt p <> " not found in environment"
    Just v -> return v
getDictionaryExp :: TypePredicate -> Deoverload HsExp
getDictionaryExp p = do
    VariableName name <- getDictionary p
    return $ HsVar $ UnQual $ HsIdent $ unpack name

makeDictName :: TypePredicate -> Deoverload VariableName
makeDictName (IsInstance (TypeVariableName cl) t) = do
    TypeVariableName suffix <- case t of
        TypeVar (TypeVariable tvn _) -> return tvn
        TypeCon (TypeConstant tcn _) -> return tcn
        TypeApp{} -> freshTypeVarName
    return $ VariableName $ "d" <> cl <> suffix


-- TODO(kc506): Dependency order: we need to process class/data/instance declarations before function definitions.
-- Can wait until we properly support data declarations, as until then we're injecting the class/instance defns manually
deoverloadModule :: HsModule -> Deoverload HsModule
deoverloadModule (HsModule a b c d decls) = do
    addKinds builtinKinds
    HsModule a b c d <$> deoverloadDecls decls

deoverloadDecls :: [HsDecl] -> Deoverload [HsDecl]
deoverloadDecls = mapM deoverloadDecl

deoverloadDecl :: HsDecl -> Deoverload HsDecl
deoverloadDecl (HsPatBind loc pat rhs ds) = HsPatBind loc pat <$> deoverloadRhs rhs <*> pure ds
deoverloadDecl _ = throwError "Unsupported declaration in deoverloader"

deoverloadRhs :: HsRhs -> Deoverload HsRhs
deoverloadRhs (HsUnGuardedRhs expr) = case expr of
    e@(HsExpTypeSig loc _ t@(HsQualType _ simpleType)) -> do
        -- Replace each constraint with a lambda for a dictionary
        ks <- getKinds
        Qualified quals _ <- synToQualType ks t
        let constraints = S.toAscList quals
        args <- mapM makeDictName constraints
        let dictArgs = M.fromList $ zip constraints args
            funArgs = [ HsPVar $ HsIdent $ unpack arg | VariableName arg <- args ]
        HsExpTypeSig _ e' _ <- addScopedDictionaries dictArgs (deoverloadExp e)
        -- Wrap the expression in a lambda that takes the dictionary arguments
        let outerType = deoverloadType t
            innerType = HsQualType [] simpleType
            -- The original expression now has the unconstrained simple type
            innerExp = HsExpTypeSig loc e' innerType
            -- The wrapping expression is a lambda taking dictionaries around the inner expression and has modified type
            outerExp = HsExpTypeSig loc (HsLambda loc funArgs innerExp) outerType
        return $ HsUnGuardedRhs $ if null constraints then innerExp else outerExp
    _ -> throwError $ "Found rhs without top-level type annotation: forgot to run type tagger?\n" <> showt expr
deoverloadRhs _ = throwError "Unsupported RHS in deoverloader"

deoverloadExp :: HsExp -> Deoverload HsExp
deoverloadExp (HsExpTypeSig l (HsVar n) t@(HsQualType origTagQuals origSimpleType)) = do
    ks <- getKinds
    let varName = convertName n
    writeLog $ "Deoverloading " <> showt varName <> ", with tagged type " <> prettyPrintT t
    requiredQuals <- tryGetType varName >>= \case
        -- Not a decl-bound name with an original type, so the tagged type must be correct. Return all constraints.
        Nothing -> mapM (synToTypePred ks) origTagQuals
        -- The tagged type is "fake": eg. `id :: a -> a` can be tagged with `id :: Num b => b -> b` if `Num b` holds. We
        -- need to make sure we only pass the dictionary arguments that the function/variable we're looking at *needs*.
        -- We get the "real" type of the name, unify the real simple type with the tagged simple type, apply the
        -- substitution to the real type, then see which constraints are actually needed to match the two constraint
        -- sets.
        Just t'@(Quantified _ (Qualified varQuals varSimpleType)) -> do
            writeLog $ "Found bound type " <> showt t'
            Qualified tagQuals tagSimpleType <- synToQualType ks t
            sub <- mgu varSimpleType tagSimpleType
            classEnv <- getClassEnvironment
            -- TODO(kc506): Currently supports finding if the instances of the "fake" type are subclasses of the
            -- "actual" type. To support actually using superclasses, we need to insert applications of functions to
            -- pull out the superclass types though, which means we need to find a path from the subclass to the
            -- superclass and translate it.
            fmap catMaybes $ forM (S.toList $ applySub sub varQuals) $ \varQual -> do
                writeLog $ showt varQual
                -- Using ifPThenBySuper would let us check for superclasses. Using entails lets us also grab any
                -- instances like `Num Int` which we'd like to keep so we know to pass a dictionary.
                p <- entails classEnv tagQuals varQual
                return $ if p then Just varQual else Nothing
    writeLog $ "Required qualifiers: " <> showt requiredQuals
    synRequiredQuals <- mapM typePredToSyn requiredQuals
    -- Replace the qualifiers on the tagged type with only the required ones, then deoverload
    let t' = deoverloadType (HsQualType synRequiredQuals origSimpleType)
        e = HsExpTypeSig l (HsVar n) t'
    -- Apply the variable to any dictionaries it needs
    addDictArgs e =<< mapM getDictionaryExp requiredQuals
-- We don't need to deoverload the constructor name, as constructors can't be qualified in Haskell 98.
deoverloadExp c@(HsCon _) = return c
deoverloadExp (HsExpTypeSig l lit@(HsLit _) (HsQualType _ t)) = return $ HsExpTypeSig l lit (HsQualType [] t)
deoverloadExp (HsExpTypeSig l (HsApp f e) _) = do
    fExp@(HsExpTypeSig _ _ t) <- deoverloadExp f
    eExp <- deoverloadExp e
    case t of
        HsQualType [] (HsTyFun _ retType) -> return $ HsExpTypeSig l (HsApp fExp eExp) (HsQualType [] retType)
        _ -> throwError $ "Got non-function type in application: " <> showt t
deoverloadExp HsInfixApp{} = throwError "Infix applications should have been replaced by the type inferrer"
deoverloadExp (HsLambda a pats e) = HsLambda a pats <$> deoverloadExp e
deoverloadExp (HsIf c e1 e2) = HsIf <$> deoverloadExp c <*> deoverloadExp e1 <*> deoverloadExp e2
deoverloadExp (HsTuple es) = HsTuple <$> mapM deoverloadExp es
deoverloadExp (HsList es) = HsList <$> mapM deoverloadExp es
deoverloadExp (HsParen e) = HsParen <$> deoverloadExp e
-- We can ignore any qualifiers: any expression needing them further down the tree will have had the dictionaries passed
-- appropriately and now just have the simple type: we should propagate this change up the tree.
deoverloadExp (HsExpTypeSig l e (HsQualType _ t)) = HsExpTypeSig l <$> deoverloadExp e <*> pure (HsQualType [] t)
deoverloadExp e = throwError $ "Unsupported expression in deoverloader: " <> showt e

-- |Convert eg. `(Num a, Monoid b) => a -> b -> ()` into `Num a -> Monoid b -> a -> b -> ()` to represent the dicts.
deoverloadType :: HsQualType -> HsQualType
deoverloadType (HsQualType constraints t) = HsQualType [] $ foldr (HsTyFun . deoverloadAsst) t constraints
deoverloadAsst :: HsAsst -> HsType
deoverloadAsst (name, args) = foldl' HsTyApp (HsTyCon name) args

-- |Given eg. `(+) :: Num a -> a -> a -> a` and `[dNuma]`, produce a type-annotated tree for the following:
-- `((+) :: Num a -> a -> a -> a) (dNuma :: Num a) :: a -> a -> a`
-- Has to take a deoverloaded type, hence the `HsType`.
addDictArgs :: MonadError Text m => HsExp -> [HsExp] -> m HsExp
addDictArgs e@HsExpTypeSig{} [] = return e
addDictArgs e@(HsExpTypeSig loc _ (HsQualType [] (HsTyFun argType retType))) (arg:args) = addDictArgs a args
    where r = HsExpTypeSig loc arg (HsQualType [] argType)
          a = HsExpTypeSig loc (HsApp e r) (HsQualType [] retType)
addDictArgs e _ = throwError $ "Invalid expression in deoverloader: " <> showt e