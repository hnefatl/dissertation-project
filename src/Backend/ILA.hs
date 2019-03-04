{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TupleSections              #-}

-- |Intermediate Language A - basically GHC's Core but without support for complicated language features like GADTs.
module Backend.ILA where

import           BasicPrelude                hiding (exp, head)
import           Control.Monad.Except        (ExceptT, MonadError, throwError)
import           Control.Monad.Reader        (MonadReader, ReaderT, asks, local, runReaderT)
import           Control.Monad.State.Strict  (MonadState, StateT, gets, modify, runStateT)
import           Data.Default                (Default)
import qualified Data.Default                as Default (def)
import           Data.Foldable               (foldrM)
import           Data.Functor                ((<&>))
import           Data.Hashable               (Hashable)
import           Data.List.Extra             (foldl', groupOn, intersperse)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Text                   as Text
import           GHC.Generics                (Generic)
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)
import           TextShow.Instances          ()
import           TextShowHsSrc               ()

import           AlphaEq                     (AlphaEq, alphaEq')
import           Backend.Deoverload          (makeDictName)
import           ExtraDefs                   (allM, inverseMap, mapError, middleText, synPrint)
import           Logger
import           NameGenerator
import           Names
import           Preprocessor.ContainedNames (ConflictInfo(..), HasBoundVariables, HasFreeVariables, getBoundVariables,
                                              getBoundVariablesAndConflicts, getFreeVariables)
import           Typechecker.Substitution    (NameSubstitution, Substitutable, Substitution(..), applySub, subCompose,
                                              subEmpty, subSingle)
import           Typechecker.Types           (Kind(..), Qualified(..), Quantified(..), QuantifiedSimpleType, Type(..),
                                              TypeConstant(..), TypePredicate(..))
import qualified Typechecker.Types           as T


-- |Datatypes are parameterised by their type name (eg. `Maybe`), a list of parametrised type variables (eg. `a`) and a
-- list of branches. Each branch is a branch name (eg. `Just` and a list of types). `data Maybe a = Nothing | Just a` is
-- `Datatype "Maybe" ["a"] [("Nothing", []), ("Just", ["a"])]`
data Strictness = Strict | NonStrict deriving (Eq, Ord, Show)
instance TextShow Strictness where
    showb = fromString . show
data Datatype = Datatype
    { typeName   :: TypeVariableName
    , parameters :: [TypeVariableName]
    , branches   :: M.Map VariableName [(Type, Strictness)] }
    deriving (Eq, Ord, Show)
instance TextShow Datatype where
    showb = fromString . show

getBranchNames :: Datatype -> S.Set VariableName
getBranchNames = M.keysSet . branches

-- TODO(kc506): Not sure if we need... maybe for instances?
data Typeclass = Typeclass
    { head    :: TypePredicate
    , methods :: M.Map VariableName QuantifiedSimpleType }
    deriving (Eq, Ord, Show)
instance TextShow Typeclass where
    showb = fromString . show


-- |A literal value
data Literal = LiteralInt Integer
             | LiteralChar Char
    deriving (Eq, Ord, Show, Generic)
instance Hashable Literal
instance TextShow Literal where
    showb (LiteralInt i)    = showb i
    showb (LiteralChar c)   = showb c

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt a = Alt AltConstructor a
    deriving (Eq, Ord, Show, Generic)
instance Hashable a => Hashable (Alt a)
instance TextShow a => TextShow (Alt a) where
    showb (Alt con e) = showb con <> " -> " <> showb e
getAltConstructor :: Alt a -> AltConstructor
getAltConstructor (Alt c _) = c
getConstructorVariables :: AltConstructor -> [VariableName]
getConstructorVariables (DataCon _ vs) = vs
getConstructorVariables _              = []
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName [VariableName] | LitCon Literal | Default
    deriving (Eq, Ord, Show, Generic)
instance Hashable AltConstructor
instance TextShow AltConstructor where
    showb (DataCon n vs) = showb n <> (if null vs then "" else " " <> args)
        where args = mconcat $ intersperse " " $ map showb vs
    showb (LitCon l)  = showb l
    showb Default     = "default"
isDefaultAlt :: Alt a -> Bool
isDefaultAlt (Alt Default _) = True
isDefaultAlt _               = False
isDataAlt :: Alt a -> Bool
isDataAlt (Alt DataCon{} _) = True
isDataAlt _                 = False
isLiteralAlt :: Alt a -> Bool
isLiteralAlt (Alt (LitCon _) _) = True
isLiteralAlt _                  = False

-- |A recursive/nonrecursive binding of a Core expression to a name.
data Binding a = NonRec VariableName a | Rec (M.Map VariableName a)
    deriving (Eq, Ord, Show, Generic)
instance Hashable a => Hashable (Binding a) where
    hashWithSalt i (NonRec v x) = hashWithSalt i (v, x)
    hashWithSalt i (Rec m)      = hashWithSalt i (M.toAscList m)
instance TextShow a => TextShow (Binding a) where
    showb (NonRec v e) = "NonRec: " <> showb v <> " = " <> showb e
    showb (Rec m) = mconcat $ intersperse "\n" $ headline:bodylines
        where (v1, e1):bs = M.toList m
              headline =    "Rec: " <> showb v1 <> " = " <> showb e1
              bodylines = [ "     " <> showb v  <> " = " <> showb e | (v, e) <- bs ]
getBindingVariables :: Binding a -> S.Set VariableName
getBindingVariables (NonRec v _) = S.singleton v
getBindingVariables (Rec m)      = M.keysSet m

-- |The AST of ILA
data Expr = Var VariableName Type -- Variable/function
          | Con VariableName Type -- Data constructor
          | Lit Literal Type
          | App Expr Expr -- Application of terms or types
          | Lam VariableName Type Expr -- Abstraction of terms or types
          | Let VariableName Type Expr Expr
          | Case Expr [VariableName] [Alt Expr] -- in `case e of [x] { a1 ; a2 ; ... }`, x is bound to e.
          | Type Type
    deriving (Eq, Ord, Show)
instance TextShow Expr where
    showb (Var n t) = showb n <> " :: " <> showb t
    showb (Con n t) = showb n <> " ;; " <> showb t
    showb (Lit l t) = showb l <> " :: " <> showb t
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> cases <> " }"
        where cases = mconcat $ intersperse " ; " $ map showb as
    showb (Type t) = "Type " <> showb t

getExprType :: MonadError Text m => Expr -> m Type
getExprType (Var _ t)              = return t
getExprType (Con _ t)              = return t
getExprType (Lit _ t)              = return t
getExprType (App e1 _)             = snd <$> (T.unwrapFun =<< getExprType e1)
getExprType (Lam _ t e)            = T.makeFun [t] =<< getExprType e
getExprType (Let _ _ _ e)          = getExprType e
getExprType (Case _ _ [])          = throwError "No alts in case"
getExprType (Case _ _ (Alt _ e:_)) = getExprType e
getExprType (Type t)               = return t

data ConverterReadableState = ConverterReadableState
    { types           :: M.Map VariableName QuantifiedSimpleType
    , renamings       :: M.Map VariableName VariableName
      -- The renamings from the renamer stage: used to get the right name for eg. tuple constructors if they've been
      -- renamed
    , topLevelRenames :: M.Map VariableName VariableName
      -- The names of all the dictionaries in the source, like `Num` and `Eq`
    , dictionaryNames :: S.Set TypeVariableName
      -- As we traverse the source we build up a mapping of available dictionaries within the current scope. This
      -- includes top-level definitions like `Num Int`, but also type-dependent ones like `Num t201`. This is needed for
      -- lowering int literals, where we need to call `fromInteger` and pass a dictionary.
    , dictionaries    :: M.Map TypePredicate VariableName }
    deriving (Eq, Show)
instance TextShow ConverterReadableState where
    showb = fromString . show
data ConverterState = ConverterState
    { datatypes        :: M.Map TypeVariableName Datatype
    , kinds            :: M.Map TypeVariableName Kind
    , reverseRenamings :: M.Map VariableName VariableName}
    deriving (Eq, Show)
instance Default ConverterState where
    def = ConverterState
        { datatypes = M.empty
        , kinds = M.empty
        , reverseRenamings = M.empty }
instance TextShow ConverterState where
    showb = fromString . show

newtype Converter a = Converter (ReaderT ConverterReadableState (StateT ConverterState (ExceptT Text (LoggerT NameGenerator))) a)
    deriving (Functor, Applicative, Monad, MonadReader ConverterReadableState, MonadState ConverterState, MonadError Text, MonadLogger, MonadNameGenerator)

runConverter :: Converter a -> M.Map VariableName VariableName -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> M.Map TypePredicate VariableName -> S.Set TypeVariableName -> ExceptT Text (LoggerT NameGenerator) (a, ConverterState)
runConverter x rs ts ks ds dNames = runStateT (runReaderT inner rState) Default.def
    where Converter inner = addKinds ks >> x
          rState = ConverterReadableState
            { renamings = M.empty, topLevelRenames = rs, types = ts, dictionaryNames = dNames, dictionaries = ds }
evalConverter :: Converter a -> M.Map VariableName VariableName -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> M.Map TypePredicate VariableName -> S.Set TypeVariableName -> ExceptT Text (LoggerT NameGenerator) a
evalConverter x rs ts ks ds dNames = fst <$> runConverter x rs ts ks ds dNames

addRenaming :: VariableName -> VariableName -> Converter a -> Converter a
addRenaming v r = addRenamings (M.singleton v r)
addRenamings :: M.Map VariableName VariableName -> Converter a -> Converter a
addRenamings rens action = do
    -- Permanently add reverse renamings for these, then run the given action with a temporarily modified state
    modify $ \st -> st { reverseRenamings = M.union (inverseMap rens) (reverseRenamings st) }
    renamedTypes <- fmap M.fromList $ forM (M.toList rens) $ \(k,v) -> (v,) <$> getType k
    local (\st -> st { renamings = M.union rens (renamings st), types = M.union renamedTypes (types st) }) action
addTypes :: M.Map VariableName QuantifiedSimpleType -> ConverterReadableState -> ConverterReadableState
addTypes ts state = state { types = M.union ts (types state) }
addKinds :: M.Map TypeVariableName Kind -> Converter ()
addKinds ks = modify $ \st -> st { kinds = M.union ks (kinds st) }
addDictionaries :: M.Map TypePredicate VariableName -> ConverterReadableState -> ConverterReadableState
addDictionaries ds state = state { dictionaries = M.union ds (dictionaries state) }
getKinds :: Converter (M.Map TypeVariableName Kind)
getKinds = gets kinds
addDatatype :: Datatype -> Converter ()
addDatatype d = modify (\s -> s { datatypes = M.insert (typeName d) d (datatypes s) })

dictionaryArgToTypePred :: Type -> Converter (Maybe TypePredicate)
dictionaryArgToTypePred c = case T.unmakeApp c of
    (TypeCon (TypeConstant conName _), [arg]) -> do
        dictNames <- asks dictionaryNames
        return $ if S.member conName dictNames then Just $ IsInstance conName arg else Nothing
    _ -> return Nothing

getRenamed :: VariableName -> Converter VariableName
getRenamed name = asks (M.lookup name . renamings) >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not in renamings."
    Just renamed -> return renamed

getRenamedOrDefault :: VariableName -> Converter VariableName
getRenamedOrDefault name = asks (M.findWithDefault name name . renamings)

getType :: VariableName -> Converter QuantifiedSimpleType
getType name = asks (M.lookup name . types) >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not in type environment"
    Just t -> return t
getSimpleType :: VariableName -> Converter Type
getSimpleType name = (\(Quantified _ t) -> t) <$> getType name
getSimpleFromSynType :: HsQualType -> Converter Type
getSimpleFromSynType t = do
    ks <- getKinds
    Qualified s t' <- T.synToQualType ks t
    unless (null s) $ throwError "Non deoverloaded function found in ILA type bindings"
    return t'

getTupleName :: Converter VariableName
getTupleName = maybe (throwError "No top-level rename found for \"(,)\"") return =<< asks (M.lookup "(,)" . topLevelRenames)

-- |Construct an expression representing a tuple given the expressions and types of each element
makeTuple :: [(Expr, Type)] -> Converter Expr
makeTuple ps = do
    let (es, ts) = unzip ps
    resultType <- T.makeTuple ts
    funType <- T.makeFun ts resultType
    makeTuple' es funType

-- |Construct an expression representing a tuple given the expressions of each element and the type of the function
-- constructing the tuple
makeTuple' :: [Expr] -> Type -> Converter Expr
makeTuple' es t = foldl' App <$> (Con <$> getTupleName <*> pure t) <*> pure es

getListNilName :: Converter VariableName
getListNilName = maybe (throwError "No top-level rename found for \"[]\"") return =<< asks (M.lookup "[]" . topLevelRenames)
getListConsName :: Converter VariableName
getListConsName = maybe (throwError "No top-level rename found for \":\"") return =<< asks (M.lookup ":" . topLevelRenames)

makeList :: [(Expr, Type)] -> Converter Expr
makeList [] = throwError "Empty list passed to makeList"
makeList ps = case S.toList uniqueTypes of
        [t] -> makeList' es t
        uts -> throwError $ "Mismatching types passed to makeList: " <> showt uts
    where (es, ts)    = unzip ps
          uniqueTypes = S.fromList ts
makeList' :: [Expr] -> Type -> Converter Expr
makeList' es t = do
    tList <- T.makeList t
    cons <- Con <$> getListConsName <*> T.makeFun [t, tList] tList
    nil  <- Con <$> getListNilName <*> pure tList
    return $ foldr (\x y -> App (App cons x) y) nil es

makeError :: Type -> Expr
makeError = Var "compilerError"

getPatRenamings :: HsPat -> Converter ([VariableName], M.Map VariableName VariableName)
getPatRenamings pat = do
    boundNames <- S.toAscList <$> getBoundVariables pat
    renames    <- M.fromList <$> mapM (\n -> (n, ) <$> freshVarName) boundNames
    return (boundNames, renames)

getPatVariableTypes :: HsPat -> Type -> Converter (M.Map VariableName Type)
getPatVariableTypes (HsPVar v   ) t = return $ M.singleton (convertName v) t
getPatVariableTypes (HsPLit _   ) _ = return M.empty
getPatVariableTypes (HsPApp con ps) t = do
    ds <- gets (M.elems . datatypes)
    case mapMaybe (M.lookup (convertName con) . branches) ds of
        [conArgs] -> do
            let argTypes = map fst conArgs
            unless (length ps == length argTypes) $ throwError $
                unwords ["Partially applied data constructor in ILA: got type", showt t, "and patterns", showt ps]
            M.unions <$> zipWithM getPatVariableTypes ps argTypes
        [] -> throwError $ "No datatype found with constructor " <> convertName con
        _ -> throwError $ "Multiple datatypes found with constructor " <> convertName con
getPatVariableTypes HsPTuple{} _ = throwError "HsPTuple should've been removed in the renamer"
getPatVariableTypes HsPList{} _ = throwError "HsPList should've been removed in the renamer"
getPatVariableTypes (HsPParen p) t = getPatVariableTypes p t
getPatVariableTypes (HsPAsPat n p) t = M.insert (convertName n) t <$> getPatVariableTypes p t
getPatVariableTypes HsPWildCard _ = return M.empty
getPatVariableTypes _           _ = throwError "Unsupported pattern"

-- |Given an ILA expression representing an application of a term x to a number of argument terms ys, return (x, ys).
unmakeApplication :: MonadError Text m => Expr -> m (Expr, [Expr])
unmakeApplication f@App{} = return $ helper f
    where helper (App e1 e2) = let (b, as) = helper e1 in (b, e2:as)
          helper e           = (e, [])
unmakeApplication e = throwError $ "Expected ILA application, got " <> showt e

toIla :: HsModule -> Converter [Binding Expr]
toIla (HsModule _ _ _ _ decls) = do
    writeLog "-------"
    writeLog "- ILA -"
    writeLog "-------"
    ts <- asks types
    forM_ (M.toList ts) $ \(v,t) -> writeLog $ showt v <> " :: " <> showt t
    let mapper e = unlines [e, unlines $ map synPrint decls]
    mapError mapper $ concat <$> mapM declToIla decls

declToIla :: HsDecl -> Converter [Binding Expr]
declToIla (HsPatBind _ pat rhs _) = do
    -- Add the types of bound variables
    (boundNames, _) <- getPatRenamings pat
    ts <- M.fromList <$> mapM (\n -> (n, ) <$> getType n) boundNames
    local (addTypes ts) $ do
        e <- rhsToIla rhs
        patToBindings pat e
declToIla (HsFunBind []) = throwError "Function definition with empty matches"
declToIla (HsFunBind (m:ms)) = do
    let getMatchName (HsMatch _ name _ _ _) = name
        getMatchArity (HsMatch _ _ ps _ _) = length ps
        funName = getMatchName m
        arity = getMatchArity m
    unless (all ((funName ==) . getMatchName) ms) $ throwError $ unlines ["Matches with different names: ", showt ms]
    unless (all ((arity ==) . getMatchArity) ms) $ throwError $ unlines ["Matches with different arities: ", showt ms]
    (argTypes, retType) <- T.unmakeFun =<< getSimpleType (convertName funName)
    args <- flip zip argTypes <$> replicateM arity freshVarName
    let matchToArg (HsMatch _ _ pats rhs _) = (pats, rhs, retType, subEmpty)
    local (addTypes $ M.map (Quantified S.empty) $ M.fromList args) $ do
        body <- patToIla args (map matchToArg (m:ms)) (makeError retType)
        return [NonRec (convertName funName) $ foldr (uncurry Lam) body args]
declToIla d@HsClassDecl{} = throwError $ "Class declaration should've been removed by the deoverloader:\n" <> synPrint d
declToIla d@(HsDataDecl _ ctx name args bs derivings) = case (ctx, derivings) of
    ([], []) -> do
        writeLog $ "Processing datatype " <> showt name
        bs' <- M.fromList <$> mapM conDeclToBranch bs
        addDatatype $ Datatype
            { typeName = convertName name
            , parameters = map convertName args
            , branches = bs' }
        let kind = foldr KindFun KindStar $ replicate (length args) KindStar
        writeLog $ "Got kind " <> showt kind
        addKinds $ M.singleton (convertName name) kind
        return []
    (_, []) -> throwError $ "Datatype contexts not supported:\n" <> showt d
    (_, _) -> throwError $ "Deriving clauses not supported:\n" <> showt d
declToIla HsTypeSig{} = return []
declToIla d = throwError $ "Unsupported declaration\n" <> showt d

conDeclToBranch :: HsConDecl -> Converter (VariableName, [(Type, Strictness)])
conDeclToBranch (HsConDecl _ name ts) = do
    ks <- getKinds
    ts' <- forM ts $ \case
        HsBangedTy t -> (, Strict) <$> T.synToType ks t
        HsUnBangedTy t -> (, NonStrict) <$> T.synToType ks t
    return (convertName name, ts')
conDeclToBranch d@HsRecDecl{} = throwError $ "Record datatypes not supported:\n" <> showt d

rhsType :: HsRhs -> Converter Type
rhsType (HsUnGuardedRhs (HsExpTypeSig _ _ t)) = getSimpleFromSynType t
rhsType (HsUnGuardedRhs _)                    = throwError "Missing explicit type sig on top-level expression in RHS"
rhsType (HsGuardedRhss _)                     = throwError "Guarded RHS not supported"
rhsToIla :: HsRhs -> Converter Expr
rhsToIla (HsUnGuardedRhs e) = expToIla e
rhsToIla (HsGuardedRhss  _) = throwError "Guarded RHS not supported"

expToIla :: HsExp -> Converter Expr
expToIla (HsExpTypeSig loc (HsVar v) t) = case v of
    Special{}          -> expToIla (HsExpTypeSig loc (HsCon v) t)
    UnQual HsSpecial{} -> expToIla (HsExpTypeSig loc (HsCon v) t)
    Qual _ HsSpecial{} -> expToIla (HsExpTypeSig loc (HsCon v) t)
    _                  -> Var <$> getRenamedOrDefault (convertName v) <*> getSimpleFromSynType t
expToIla (HsExpTypeSig _ (HsCon c) t) = Con (convertName c) <$> getSimpleFromSynType t
expToIla (HsExpTypeSig _ (HsLit l) t) = litToIlaExpr l =<< getSimpleFromSynType t
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla HsInfixApp{} = throwError "Infix applications not supported: should've been removed by the typechecker"
expToIla (HsLambda _ [] e) = expToIla e
-- TODO(kc506): Rewrite to not use the explicit type signature: get the types of the subexpressions instead
expToIla (HsExpTypeSig l (HsLambda l' pats e) t) = do
    argNames <- replicateM (length pats) freshVarName
    expType <- getSimpleFromSynType t
    (argTypes, _) <- T.unmakeFun expType
    renames <- M.unions . map snd <$> mapM getPatRenamings pats
    patVariableTypes <- fmap (Quantified S.empty) . M.unions <$> zipWithM getPatVariableTypes pats argTypes
    let renamedVariableTypes = M.mapKeys (renames M.!) patVariableTypes
        log_renames = Text.intercalate ", " $ map (uncurry $ middleText "/") $ M.toList renames
        log_types = Text.intercalate ", " $ map (uncurry $ middleText " :: ") $ M.toList renamedVariableTypes
    writeLog $ "Lambda pattern: added [" <> log_renames <> "] and [" <> log_types <> "]"
    -- We need to keep track of any dictionaries being passed in, so lower functions can use them.
    let convert (_, Nothing)   = Nothing
        convert (n, Just pred) = Just (pred, n)
    dictionaryArgs <- M.fromList . catMaybes . map convert . zip argNames <$> mapM dictionaryArgToTypePred argTypes
    -- The body of this lambda is constructed by wrapping the next body with pattern match code
    body <- local (addTypes patVariableTypes . addDictionaries dictionaryArgs) $ addRenamings renames $
        patToIla (zip argNames argTypes) [(pats, HsUnGuardedRhs e, expType, subEmpty)] (makeError expType)
    return $ foldr (uncurry Lam) body (zip argNames argTypes)
expToIla HsLambda{} = throwError "Lambda with body not wrapped in explicit type signature"
--expToIla (HsLet [] e) = expToIla e
--expToIla (HsLet (d:ds) e) = do
--    bs <- declToIla d
--    body <- expToIla (HsLet ds e)
--    let processBinding (NonRec v e') body' = writeLog (showt v <> " " <> showt e') >> Let v <$> getSimpleType v <*> pure e' <*> pure body'
--        processBinding (Rec m) body' = foldrM (\(k,v) body'' -> processBinding (NonRec k v) body'') body' (M.toList m)
--    foldrM processBinding body bs
expToIla (HsIf cond e1 e2) = do
    condExp <- expToIla cond
    e1Exp   <- expToIla e1
    e2Exp   <- expToIla e2
    let alts = [ Alt (DataCon "True" []) e1Exp , Alt (DataCon "False" []) e2Exp ]
    return $ Case condExp [] alts
expToIla (HsCase scrut alts) = do
    scrut' <- expToIla scrut
    alts' <- mapM altToIla alts
    return $ Case scrut' [] alts'
expToIla HsTuple{} = throwError "HsTuple should've been removed in renamer"
expToIla HsList{} = throwError "HsList should've been removed in renamer"
expToIla (HsParen exp) = expToIla exp
expToIla (HsExpTypeSig _ e _) = expToIla e
expToIla e = throwError $ "Unsupported expression: " <> showt e

-- Big TODO(kc506): Need to handle any pattern, not just this subset. So need to find a way to reuse patToIla to
-- construct alts.... Handle anything other than these types by using patToIla and using altToIla in the body?
altToIla :: HsAlt -> Converter (Alt Expr)
altToIla (HsAlt _ pat alts wheres) = helper pat
    where helper p = case p of
            HsPApp con vs -> Alt (DataCon (convertName con) (map patToVar vs)) <$> guardedAltsToIla alts
            HsPInfixApp p1 con p2 -> Alt (DataCon (convertName con) [patToVar p1, patToVar p2]) <$> guardedAltsToIla alts
            HsPLit l      -> Alt <$> (LitCon <$> litToIla l) <*> guardedAltsToIla alts
            HsPWildCard   -> Alt Default <$> guardedAltsToIla alts
            HsPParen p'   -> helper p'
            _             -> throwError $ unlines ["Case expression with non-constructor-application pattern:", showt pat]
          patToVar (HsPVar v) = convertName v
          patToVar _          = error "Non-variable in ILA, need to rework case alt patterns"

guardedAltsToIla :: HsGuardedAlts -> Converter Expr
guardedAltsToIla (HsUnGuardedAlt e) = expToIla e
guardedAltsToIla HsGuardedAlts{}    = throwError "Guarded alts not supported in ILA" -- Should be able to rewrite into a chain of if expressions (so a chain of case expressions)

-- TODO(kc506): Remove once we've changed altToIla to use patToIla, as this is unused
litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c)   = return $ LiteralChar c
litToIla (HsInt i)    = return $ LiteralInt i
litToIla l            = throwError $ "Unboxed primitives not supported: " <> showt l

-- |Convert a HsLiteral to an equivalent ILA literal
litToIlaExpr :: HsLiteral -> Type -> Converter Expr
litToIlaExpr (HsChar c) t = return $ Lit (LiteralChar c) t
litToIlaExpr (HsString s) t = case T.unmakeApp t of
    (conType, [elemType])
        | conType /= T.typeList -> throwError $ "Expected list type, got: " <> showt t
        | otherwise -> do
            -- Convert each char to a literal, make an ILA list of the results
            ls <- mapM (flip litToIlaExpr elemType . HsChar) s
            makeList' ls elemType
    _ -> throwError $ "Expected list type, got: " <> showt t
litToIlaExpr HsFrac{} _ = throwError "Rationals not supported in ILA"
litToIlaExpr (HsInt i) t = asks (M.lookup "fromInteger" . topLevelRenames) >>= \case
    Just funFromInteger -> asks (M.lookup (IsInstance "Num" t) . dictionaries) >>= \case
        Just dictName -> do
            let integerType = TypeCon $ TypeConstant "Integer" KindStar
                dictType = TypeApp (TypeCon $ TypeConstant "Num" $ KindFun KindStar KindStar) t KindStar
            funFromIntegerType <- T.makeFun [dictType, integerType] t
            return $ App (App (Var funFromInteger funFromIntegerType) (Var dictName dictType)) (Lit (LiteralInt i) integerType)
        Nothing -> throwError $ "No dictionary found for " <> showt (IsInstance "Num" t)
    Nothing -> throwError "No renaming for fromInteger."
litToIlaExpr l _ = throwError $ "Unboxed primitives not supported: " <> synPrint l


-- |Wrapper around patToBindings' that ensures we don't generate two bindings for simple variable bindings.
patToBindings :: HsPat -> Expr -> Converter [Binding Expr]
patToBindings p@HsPVar{} e = mapM ($ e) =<< patToBindings' p
patToBindings p e = do
    -- We're going to generate more than one binding: avoid duplicating the head expression in each by binding it to a
    -- main variable
    v <- freshVarName
    eType <- getExprType e
    fmap (NonRec v e:) . mapM ($ Var v eType) =<< patToBindings' p

-- |Generates bindings of values to variables from the given pattern and expression. Essentially traverse the pattern
-- tree looking for bound variables, then creates a binding that unwraps the given expression down to the value bound to
-- the pattern.
-- TODO(kc506): Pretty sure we can just bind the expression to a main value then use patToIla to generate
-- the secondary bindings...
patToBindings' :: MonadError Text m => HsPat -> m [Expr -> Converter (Binding Expr)]
patToBindings' (HsPVar v) = return [ \e -> NonRec <$> (getRenamedOrDefault $ convertName v) <*> pure e ]
patToBindings' HsPWildCard = return []
patToBindings' (HsPParen p) = patToBindings' p
patToBindings' (HsPAsPat n p) = (return . NonRec (convertName n):) <$> patToBindings' p
patToBindings' (HsPApp con ps) = do
    newBindingConts <- concat . zipWith (\i -> map (i,)) [0..] <$> mapM patToBindings' ps
    let wrapInCase :: (Int, Expr -> Converter (Binding Expr)) -> Expr -> Converter (Binding Expr)
        wrapInCase (i, c) e = do
            -- TODO(kc506): rather than generating loads of new variable names, use Nothing/Just.
            vs <- replicateM (length ps) freshVarName
            let v = vs !! i
            eType <- getExprType e
            c (Var v eType) >>= \case
              NonRec b e' -> return $ NonRec b $ Case e [] [ Alt (DataCon (convertName con) vs) e', Alt Default $ makeError eType ]
              Rec{} -> throwError "Recursive bindings not supported in patToBindings'"
    return $ map wrapInCase newBindingConts
patToBindings' (HsPInfixApp p1 c p2) = patToBindings' $ HsPApp c [p1, p2]
patToBindings' HsPTuple{} = throwError "HsPTuple should've been replaced already"
patToBindings' HsPList{} = throwError "HsPList should've been replaced already"
patToBindings' HsPLit{} = throwError "HsPLit in decl patterns not yet supported"
patToBindings' HsPNeg{} = throwError "HsPNeg in decl patterns not yet supported"
patToBindings' HsPRec{} = throwError "HsPRec in decl patterns not yet supported"
patToBindings' HsPIrrPat{} = throwError "HsPIrrPat in decl patterns not yet supported"

-- Implemented as in "The Implementation of Functional Programming Languages"
patToIla :: [(VariableName, Type)] -> [([HsPat], HsRhs, Type, NameSubstitution)] -> Expr -> Converter Expr
patToIla _ [] def = return def
patToIla [] (([], r, _, Substitution sub):_) _ =
    -- TODO(kc506): If the first case can fail somehow, we need to use the later ones? Weird notation in the book
    addRenamings sub $ rhsToIla r
patToIla ((v, t):vs) cs def = do
    let getPat (x:_, _, _, _) = x
        getPat ([], _, _, _)  = error "Empty initial pattern in patToIla"
        allPatType pt = allM (fmap (== pt) . getPatType . getPat)
    pats <- mapM (reducePats v) cs
    allVariable <- allPatType Variable pats
    allLiteral <- allPatType Literal pats
    allCon <- allPatType Constructor pats
    if allVariable then do
        -- Extract the variable names from the patterns, get the new cases
        (pats', varNames) <- fmap unzip $ forM pats $ \(pl, e, et, sub) -> case pl of
            HsPVar n:ps -> return ((ps, e, et, subCompose sub $ subSingle (convertName n) v), Just $ convertName n)
            HsPWildCard:ps -> return ((ps, e, et, sub), Nothing)
            [] -> throwError "Internal: wrong patterns length"
            p:_ -> throwError $ "Non-HsPVar in variables: " <> showt p
        let newTypes = M.fromList $ zip (catMaybes varNames) (repeat $ Quantified S.empty t)
        -- Keep track of dictionary arguments being passed in
        dictionaryArgs <- dictionaryArgToTypePred t <&> \case
            Nothing -> M.empty
            Just pred -> M.singleton pred v
        local (addTypes newTypes . addDictionaries dictionaryArgs) $
            patToIla vs pats' def
    else if allLiteral then do
        -- To check literals we create a right-leaning tree of case statements, like:
        -- > case x == 0 of { True -> E1 ; False -> case x == 1 of { ... } }
        rs <- asks topLevelRenames
        eqDictPlainName <- makeDictName $ IsInstance "Eq" t -- Might need to be renamed with a substitution below
        eqDictType <- T.makeApp (T.TypeCon $ T.TypeConstant "Eq" (KindFun KindStar KindStar)) [t]
        case (M.lookup "True" rs, M.lookup "False" rs, M.lookup "==" rs) of
            (Nothing, _, _) -> throwError "True constructor not found in ILA lowering"
            (_, Nothing, _) -> throwError "False constructor not found in ILA lowering"
            (_, _, Nothing) -> throwError "(==) function not found in ILA lowering"
            (Just trueName, Just falseName, Just equals) -> do
                equalsType <- T.makeFun [eqDictType, t, t] T.typeBool
                let trueCon = DataCon trueName []
                    falseCon = DataCon falseName []
                    equalsFun = Var equals equalsType
                    customFoldM d xs f = foldrM f d xs
                customFoldM def pats $ \(pl, rhs, _, sub) body -> case pl of
                    [HsPLit l] -> do
                        l' <- litToIlaExpr l t
                        let cond = foldl App equalsFun [eqDict, Var v t, l']
                            eqDictName = applySub sub eqDictPlainName
                            eqDict = Var eqDictName eqDictType
                        expr <- rhsToIla rhs
                        return $ Case cond [] [ Alt trueCon (applySub sub expr), Alt falseCon body ]
                    p -> throwError $ "Expected single literal in ILA lowering, got: " <> showt p
    else if allCon then do
        conGroups <- groupPatCons getPat pats
        alts <- forM conGroups $ \(con, (HsPApp _ newpats:ps, r, t, s):es) -> do
            cont <- asks (M.lookup con . types) >>= \case
                Just (Quantified _ t) -> return t
                Nothing -> throwError $ "No type for constructor " <> showt con
            let (argTypes, _) = T.unmakeCon cont
            freshArgVars <- replicateM (length argTypes) freshVarName
            let freshArgTypes = zip freshArgVars argTypes
            local (addTypes $ M.map (Quantified S.empty) $ M.fromList freshArgTypes) $ do
                body <- patToIla (freshArgTypes <> vs) ((newpats <> ps, r, t, s):es) def
                return $ Alt (DataCon con freshArgVars) body
        let defaultAlt = Alt Default def
        return $ Case (Var v t) [] (defaultAlt:alts)
    else do -- Mixture of variables/literals/constructors
        patGroups <- map snd <$> groupPatsOn getPat pats
        foldrM (patToIla $ (v,t):vs) def patGroups
patToIla a b c = throwError $ unlines ["patToIla bug: ", showt a, showt b, showt c]

-- Get the type of each pattern:
data PatType = Variable | Constructor | Literal
    deriving (Eq)
getPatType :: MonadError Text m => HsPat -> m PatType
getPatType HsPVar{}    = return Variable
getPatType HsPWildCard = return Variable
getPatType HsPLit{}    = return Literal
getPatType HsPApp{}    = return Constructor
getPatType p           = throwError $ "Illegal pattern " <> showt p

-- |Group items by their pattern type
groupPatsOn :: MonadError Text m => (a -> HsPat) -> [a] -> m [(PatType, [a])]
groupPatsOn f xl = do
    grouped <- groupOn fst <$> mapM (\x -> fmap (,x) $ getPatType $ f x) xl
    return $ map (\((p, x):xs) -> (p, x:map snd xs)) grouped

-- |Reorder and group the items so that each group contains all items with the same constructor
groupPatCons :: (MonadError Text m, Eq a) => (a -> HsPat) -> [a] -> m [(VariableName, [a])]
groupPatCons f xl = do
    tagged <- forM xl $ \x -> case f x of
        HsPApp n _ -> return (convertName n, x)
        _          -> throwError "Non-constructor in rearrangePatCons"
    return $ map (\((v, x):xs) -> (v, x:map snd xs)) $ groupOn fst $ sortOn fst tagged

-- |Reduce patterns so that the root of each pattern is a variable/literal/constructor
reducePats :: MonadError Text m => VariableName -> ([HsPat], HsRhs, Type, NameSubstitution) -> m ([HsPat], HsRhs, Type, NameSubstitution)
reducePats _ x@([], _, _, _) = return x
reducePats v (p:ps, e, t', s) = case p of
    HsPInfixApp x1 op x2 -> return (HsPApp op [x1, x2]:ps, e, t', s)
    HsPParen x           -> reducePats v (x:ps, e, t', s)
    HsPAsPat n x         -> reducePats v (x:ps, e, t', subCompose s $ subSingle (convertName n) v)
    HsPTuple{}           -> throwError "HsPTuple should've been removed in the renamer"
    HsPList{}            -> throwError "HsPList should've been removed in the renamer"
    _                    -> return (p:ps, e, t', s)


instance HasFreeVariables a => HasFreeVariables (Alt a) where
    getFreeVariables (Alt c e) = S.difference <$> getFreeVariables e <*> getFreeVariables c
instance HasFreeVariables AltConstructor where
    getFreeVariables Default        = return S.empty
    getFreeVariables LitCon{}       = return S.empty
    getFreeVariables (DataCon _ vs) = return $ S.fromList vs
instance HasBoundVariables (Alt a) where
    getBoundVariablesAndConflicts (Alt c _) = getBoundVariablesAndConflicts c
instance HasBoundVariables AltConstructor where
    getBoundVariablesAndConflicts Default          = return M.empty
    getBoundVariablesAndConflicts LitCon{}         = return M.empty
    getBoundVariablesAndConflicts (DataCon con vs) = return $ M.fromList $ zip (con:vs) (repeat $ S.singleton SymDef)
instance HasBoundVariables (Binding a) where
    getBoundVariablesAndConflicts (NonRec v _) = return $ M.singleton v (S.singleton SymDef)
    getBoundVariablesAndConflicts (Rec m)      = return $ M.fromSet (const $ S.singleton SymDef) (M.keysSet m)
instance HasFreeVariables a => HasFreeVariables (Binding a) where
    getFreeVariables (NonRec v e) = S.delete v <$> getFreeVariables e
    getFreeVariables (Rec m)      = fmap S.unions $ forM (M.toList m) $ \(v, e) -> S.delete v <$> getFreeVariables e

instance Substitutable VariableName VariableName Expr where
    applySub (Substitution sub) (Var v t) = Var (M.findWithDefault v v sub) t
    applySub (Substitution sub) (Con v t) = Con (M.findWithDefault v v sub) t
    applySub _ l@Lit{}                    = l
    applySub sub (App e1 e2)              = App (applySub sub e1) (applySub sub e2)
    applySub sub (Lam v t e)              = Lam v t (applySub sub e)
    applySub sub (Let v t e b)            = Let v t (applySub sub e) (applySub sub b)
    applySub sub (Case e vs as)           = Case (applySub sub e) vs (applySub sub as)
    applySub _ t@Type{}                   = t
instance Substitutable a b c => Substitutable a b (Alt c) where
    applySub sub (Alt c x) = Alt c (applySub sub x)

instance (AlphaEq a, Ord a) => AlphaEq (Binding a) where
    alphaEq' (NonRec v1 e1) (NonRec v2 e2) = alphaEq' v1 v2 >> alphaEq' e1 e2
    alphaEq' (Rec m1) (Rec m2)             = alphaEq' m1 m2
    alphaEq' b1 b2                         = throwError $ unlines [ "Binding mismatch:", showt b1, "vs", showt b2 ]
instance AlphaEq a => AlphaEq (Alt a) where
    alphaEq' (Alt ac1 e1) (Alt ac2 e2) = alphaEq' ac1 ac2 >> alphaEq' e1 e2
instance AlphaEq Literal where
    alphaEq' (LiteralInt i1) (LiteralInt i2) =
        unless (i1 == i2) $ throwError $ "Integer literal mismatch:" <> showt i1 <> " vs " <> showt i2
    alphaEq' (LiteralChar c1) (LiteralChar c2) =
        unless (c1 == c2) $ throwError $ "Character literal mismatch:" <> showt c1 <> " vs " <> showt c2
    alphaEq' l1 l2 = throwError $ "Literal mismatch:" <> showt l1 <> " vs " <> showt l2
instance AlphaEq AltConstructor where
    alphaEq' (DataCon con1 vs1) (DataCon con2 vs2) = alphaEq' con1 con2 >> alphaEq' vs1 vs2
    alphaEq' (LitCon l1) (LitCon l2) = alphaEq' l1 l2
    alphaEq' Default Default = return ()
    alphaEq' c1 c2 = throwError $ unlines [ "Alt constructor mismatch:", showt c1, "vs", showt c2 ]
instance AlphaEq Expr where
    alphaEq' (Var n1 t1) (Var n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (Con n1 t1) (Con n2 t2) = alphaEq' n1 n2 >> alphaEq' t1 t2
    alphaEq' (Lit l1 t1) (Lit l2 t2) = alphaEq' l1 l2 >> alphaEq' t1 t2
    alphaEq' (App e1a e1b) (App e2a e2b) = alphaEq' e1a e2a >> alphaEq' e1b e2b
    alphaEq' (Lam v1 t1 e1) (Lam v2 t2 e2) = alphaEq' v1 v2 >> alphaEq' t1 t2 >> alphaEq' e1 e2
    alphaEq' (Let v1 t1 e1a e1b) (Let v2 t2 e2a e2b) = do
        alphaEq' v1 v2
        alphaEq' t1 t2
        alphaEq' e1a e2a
        alphaEq' e1b e2b
    alphaEq' (Case e1 vs1 as1) (Case e2 vs2 as2) = alphaEq' e1 e2 >> alphaEq' vs1 vs2 >> alphaEq' as1 as2
    alphaEq' (Type t1) (Type t2) = alphaEq' t1 t2
    alphaEq' e1 e2 = throwError $ unlines [ "ILA Expression mismatch:", showt e1, "vs", showt e2 ]
