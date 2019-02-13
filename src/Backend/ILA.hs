{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

-- |Intermediate Language A - basically GHC's Core but without support for complicated language features like GADTs.
module Backend.ILA where

import           BasicPrelude                hiding (exp, head)
import           Control.Monad.Except        (ExceptT, MonadError, throwError)
import           Control.Monad.Reader        (MonadReader, ReaderT, asks, local, runReaderT)
import           Control.Monad.State.Strict  (MonadState, StateT, gets, modify, runStateT)
import           Data.Default                (Default, def)
import           Data.Foldable               (foldrM)
import           Data.List.Extra             (foldl', intersperse, groupOn)
import           Data.Tuple.Extra            (fst3, snd3, thd3)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Text                   as Text
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)
import           TextShow.Instances          ()
import           TextShowHsSrc               ()

import           ExtraDefs                   (inverseMap, mapError, middleText, synPrint, allM)
import           Logger
import           NameGenerator
import           Names
import           Preprocessor.ContainedNames (ConflictInfo(..), HasBoundVariables, HasFreeVariables, getBoundVariables,
                                              getBoundVariablesAndConflicts, getFreeVariables)
import           Typechecker.Types           (Kind(..), Qualified(..), Quantified(..), QuantifiedSimpleType, Type(..),
                                              TypePredicate(..))
import qualified Typechecker.Types           as T
import Typechecker.Substitution (Substitutable, NameSubstitution, Substitution(..), applySub, subCompose, subComposes, subSingle, subEmpty)


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
             | LiteralFrac Rational
             | LiteralChar Char
             | LiteralString String
    deriving (Eq, Ord, Show)
instance TextShow Literal where
    showb (LiteralInt i)    = showb i <> " :: Int"
    showb (LiteralFrac r)   = showb r <> " :: Rational"
    showb (LiteralChar c)   = showb c <> " :: Char"
    showb (LiteralString s) = showb s <> " :: String"

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt a = Alt AltConstructor a
    deriving (Eq, Ord, Show)
instance TextShow a => TextShow (Alt a) where
    showb (Alt con e) = showb con <> " -> " <> showb e
getAltConstructor :: Alt a -> AltConstructor
getAltConstructor (Alt c _) = c
getConstructorVariables :: AltConstructor -> [VariableName]
getConstructorVariables (DataCon _ vs) = vs
getConstructorVariables _              = []
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName [VariableName] | LitCon Literal | Default
    deriving (Eq, Ord, Show)
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
    deriving (Eq, Ord, Show)
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
getExprType (Lam _ t e)            = T.makeFun [t] <$> getExprType e
getExprType (Let _ _ _ e)          = getExprType e
getExprType (Case _ _ [])          = throwError "No alts in case"
getExprType (Case _ _ (Alt _ e:_)) = getExprType e
getExprType (Type t)               = return t

data ConverterReadableState = ConverterReadableState
    { types           :: M.Map VariableName QuantifiedSimpleType
    , renamings       :: M.Map VariableName VariableName
      -- The renamings from the renamer stage: used to get the right name for eg. tuple constructors if they've been
      -- renamed
    , topLevelRenames :: M.Map VariableName VariableName }
    deriving (Eq, Show)
instance TextShow ConverterReadableState where
    showb = fromString . show
data ConverterState = ConverterState
    { datatypes        :: M.Map TypeVariableName Datatype
    , kinds            :: M.Map TypeVariableName Kind
    , reverseRenamings :: M.Map VariableName VariableName }
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

runConverter :: Converter a -> M.Map VariableName VariableName -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> ExceptT Text (LoggerT NameGenerator) (a, ConverterState)
runConverter x rs ts ks = runStateT (runReaderT inner rState) def
    where Converter inner = addKinds ks >> x
          rState = ConverterReadableState { renamings = M.empty, topLevelRenames = rs, types = ts }
evalConverter :: Converter a -> M.Map VariableName VariableName -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> ExceptT Text (LoggerT NameGenerator) a
evalConverter x rs ts ks = fst <$> runConverter x rs ts ks

addRenaming :: VariableName -> VariableName -> Converter a -> Converter a
addRenaming v r = addRenamings (M.singleton v r)
addRenamings :: M.Map VariableName VariableName -> Converter a -> Converter a
addRenamings rens action = do
    -- Permanently add reverse renamings for these, then run the given action with a temporarily modified state
    modify $ \st -> st { reverseRenamings = M.union (inverseMap rens) (reverseRenamings st) }
    writeLog . showt =<< asks types
    renamedTypes <- fmap M.fromList $ forM (M.toList rens) $ \(k,v) -> (v,) <$> getType k
    local (\st -> st { renamings = M.union rens (renamings st), types = M.union renamedTypes (types st) }) ((writeLog . showt =<< asks types) >> action)
addTypes :: M.Map VariableName QuantifiedSimpleType -> ConverterReadableState -> ConverterReadableState
addTypes ts state = state { types = M.union ts (types state) }
addKinds :: M.Map TypeVariableName Kind -> Converter ()
addKinds ks = modify $ \st -> st { kinds = M.union ks (kinds st) }
getKinds :: Converter (M.Map TypeVariableName Kind)
getKinds = gets kinds
addDatatype :: Datatype -> Converter ()
addDatatype d = modify (\s -> s { datatypes = M.insert (typeName d) d (datatypes s) })

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
makeTuple ps = makeTuple' es t
  where (es, ts) = unzip ps
        t        = T.makeFun ts (T.makeTuple ts)
-- |Construct an expression representing a tuple given the expressions of each element and the type of the tuple
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
    cons <- Con <$> getListConsName <*> pure (T.makeFun [t, T.makeList t] $ T.makeList t)
    nil  <- Con <$> getListNilName <*> pure (T.makeList t)
    return $ foldr (\x y -> App (App cons x) y) nil es

makeError :: Type -> Expr
makeError = Var "compilerError"

getPatRenamings :: HsPat -> Converter ([VariableName], M.Map VariableName VariableName)
getPatRenamings pat = do
    boundNames <- S.toAscList <$> getBoundVariables pat
    renames    <- M.fromList <$> mapM (\n -> (n, ) <$> freshVarName) boundNames
    return (boundNames, renames)

getPatVariableTypes :: MonadError Text m => HsPat -> Type -> m (M.Map VariableName Type)
getPatVariableTypes (HsPVar v   ) t = return $ M.singleton (convertName v) t
getPatVariableTypes (HsPLit _   ) _ = return M.empty
getPatVariableTypes (HsPApp _ ps) t = do
    let (_, argTypes) = T.unmakeApp t
    unless (length ps == length argTypes) $ throwError "Partially applied data constructor in ILA lowering"
    M.unions <$> zipWithM getPatVariableTypes ps argTypes
getPatVariableTypes (HsPTuple ps) t = throwError "HsPTuple should've been removed in the renamer"
getPatVariableTypes (HsPList ps) t = throwError "HsPList should've been removed in the renamer"
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
    -- Precompute a mapping from the bound names in the patterns to some fresh names
    (boundNames, renames) <- getPatRenamings pat
    let boundNameLength = length boundNames
    boundTypes <- mapM getSimpleType boundNames -- Type of each bound name
    writeLog $ "Found bound names: " <> Text.intercalate " " (zipWith (middleText " :: ") boundNames boundTypes)
    -- Create expressions for each of the fresh names
    let renamedExps = zipWith (\name t -> Var (renames M.! name) t) boundNames boundTypes
    -- Generate an expression that matches the patterns then returns a tuple of every variable
    let resultType  = T.makeTuple boundTypes
    resultTuple <- makeTuple (zip renamedExps boundTypes)
    ts <- M.fromList <$> mapM (\n -> (n, ) <$> getType n) boundNames
    resultExpr <- local (addTypes ts) $ addRenamings renames $ do
        rhsExpr <- rhsToIla rhs -- Get an expression for the RHS using the renamings from actual name to temporary name
        rhst <- rhsType rhs
        argName <- freshVarName
        patToIla [(argName, rhst)] [([pat], rhsExpr, rhst, subEmpty)] (makeError rhst)
    -- The variable name used to store the result tuple: each bound name in the patterns pulls their values from this
    resultName <- freshVarName
    -- For each bound name, generate a binding that extracts the variable from the result tuple
    let extractorMap (name, index) = do
            bindingType <- getSimpleType name -- Get the type of this bound variable
            tempNames   <- replicateM boundNameLength freshVarName -- Create temporary variables for pattern matching on
            let tempName = tempNames !! index -- Get the temporary name in the right position in the tuple
                body = Var tempName bindingType
            -- Just retrieve the specific output variable
            tupleName <- getTupleName
            return $ NonRec name $ Case (Var resultName resultType) [] [ Alt (DataCon tupleName tempNames) body , Alt Default (makeError bindingType) ]
    extractors <- mapM extractorMap (zip boundNames [0 ..])
    return $ Rec (M.singleton resultName resultExpr):extractors
declToIla (HsFunBind matches) = throwError "Functions not supported in ILA"
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
expToIla (HsExpTypeSig _ (HsLit l) t) = Lit <$> litToIla l <*> getSimpleFromSynType t
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla HsInfixApp{} = throwError "Infix applications not supported: should've been removed by the typechecker"
expToIla (HsLambda _ [] e) = expToIla e
-- TODO(kc506): Rewrite to not use the explicit type signature: get the types of the subexpressions instead
expToIla (HsExpTypeSig l (HsLambda l' (p:pats) e) t) = do
    argName <- freshVarName
    expType <- getSimpleFromSynType t
    (argType, bodyType) <- T.unwrapFun expType
    reducedType <- T.qualTypeToSyn $ Qualified S.empty bodyType
    (_, renames) <- getPatRenamings p
    patVariableTypes <- fmap (Quantified S.empty) <$> getPatVariableTypes p argType
    let renamedVariableTypes = M.mapKeys (renames M.!) patVariableTypes
        log_renames = Text.intercalate ", " $ map (uncurry $ middleText "/") $ M.toList renames
        log_types = Text.intercalate ", " $ map (uncurry $ middleText " :: ") $ M.toList renamedVariableTypes
    writeLog $ "Lambda pattern: added [" <> log_renames <> "] and [" <> log_types <> "]"
    -- The body of this lambda is constructed by wrapping the next body with pattern match code
    body <- local (addTypes patVariableTypes) $ addRenamings renames $ do
        nextBody <- expToIla (HsExpTypeSig l (HsLambda l' pats e) reducedType)
        --patToIla p argType (Var argName argType) nextBody bodyType
        -- Probably just need a single lambda processor now? Can handle multiple pattern so.
        patToIla [(argName, rhst)] [([pat], rhsExpr, rhst, subEmpty)] (makeError rhst)
    return (Lam argName argType body)
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

litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c)   = return $ LiteralChar c
litToIla (HsString s) = return $ LiteralString s
litToIla (HsInt i)    = return $ LiteralInt i  -- Replace with fromInteger to cast to arbitrary Num?
litToIla (HsFrac r)   = return $ LiteralFrac r
litToIla l            = throwError $ "Unboxed primitives not supported: " <> showt l


-- Implemented as in "The Implementation of Functional Programming Languages"
patToIla :: [(VariableName, Type)] -> [([HsPat], Expr, Type, NameSubstitution)] -> Expr -> Converter Expr
patToIla _ [] def = return def
patToIla [] (([], e, _, sub):_) _ =
    -- TODO(kc506): If the first case can fail somehow, we need to use the later ones? Weird notation in the book
    return $ applySub sub e
patToIla ((v, t):vs) cs def = do
    let getPat (x:_, _, _, _) = x
        allPatType pt = allM (fmap (== pt) . getPatType . getPat)
    pats <- mapM (reducePats v) cs
    allVariable <- allPatType Variable pats
    allLiteral <- allPatType Literal pats
    allCon <- allPatType Constructor pats
    if allVariable then do
        -- Extract the variable names `ns` from the patterns, get the new cases `cs'`
        cs' <- forM cs $ \(pl, e, et, sub) -> case pl of
            HsPVar n:ps -> return (ps, e, et, subCompose sub $ subSingle (convertName n) v)
            [] -> throwError "Internal: wrong patterns length"
            _ -> throwError "Non-HsPVar in variables"
        patToIla vs cs' def
    else if allLiteral then
        -- Not supported yet...
        throwError "Literals in pattern match in ILA"
    else if allCon then do
        conGroups <- groupPatCons getPat cs
        alts <- forM conGroups $ \(con, es) -> do
            cont <- asks (M.lookup con . types) >>= \case
                Just (Quantified _ t) -> return t
                Nothing -> throwError $ "No type for constructor " <> showt con
            (args, _) <- T.unmakeFun cont
            freshArgVars <- replicateM (length args) freshVarName
            body <- patToIla (zip freshArgVars args <> vs) es def
            return $ Alt (DataCon con freshArgVars) body
        let defaultAlt = Alt Default def
        return $ Case (Var v t) [] (defaultAlt:alts)
    else -- Mixture of variables/literals/constructors
        throwError "Not implemented yet in patToIla"

-- Get the type of each pattern:
data PatType = Variable | Constructor | Literal
    deriving (Eq)
getPatType :: MonadError Text m => HsPat -> m PatType
getPatType HsPVar{} = return Variable
getPatType HsPWildCard = return Variable
getPatType HsPLit{} = return Literal
getPatType HsPApp{} = return Constructor
getPatType p = throwError $ "Illegal pattern " <> showt p

-- |Group items by their pattern type
groupPatsOn :: MonadError Text m => (a -> HsPat) -> [a] -> m [(PatType, [a])]
groupPatsOn f xl = do
    grouped <- groupOn fst <$> mapM (\x -> fmap (,x) $ getPatType $ f x) xl
    return $ map (\((p, x):xs) -> (p, x:map snd xs)) grouped

-- |Reorder and group the items so that each group contains all items with the same constructor
groupPatCons :: (MonadError Text m, Eq a) => (a -> HsPat) -> [a] -> m [(VariableName, [a])]
groupPatCons f xl = do
    tagged <- forM xl $ \x -> case f x of
        HsPApp n ps -> return (convertName n, x)
        _ -> throwError "Non-constructor in rearrangePatCons"
    return $ map (\((v, x):xs) -> (v, x:map snd xs)) $ groupOn fst $ sortBy (comparing fst) tagged

-- |Reduce patterns so that the root of each pattern is a variable/literal/constructor
reducePats :: MonadError Text m => VariableName -> ([HsPat], Expr, Type, NameSubstitution) -> m ([HsPat], Expr, Type, NameSubstitution)
reducePats _ x@([], _, _, _) = return x
reducePats v (p:ps, e, t', s) = case p of
    HsPInfixApp x1 op x2 -> return (HsPApp op [x1, x2]:ps, e, t', s)
    HsPParen x -> reducePats v (x:ps, e, t', s)
    HsPAsPat n x -> reducePats v (x:ps, e, t', subCompose s $ subSingle (convertName n) v)
    HsPTuple{} -> throwError "HsPTuple should've been removed in the renamer"
    HsPList{} -> throwError "HsPList should've been removed in the renamer"
    _ -> return (p:ps, e, t', s)


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
    applySub _ l@Lit{} = l
    applySub sub (App e1 e2) = App (applySub sub e1) (applySub sub e2)
    applySub sub (Lam v t e) = Lam v t (applySub sub e)
    applySub sub (Let v t e b) = Let v t (applySub sub e) (applySub sub b)
    applySub sub (Case e vs as) = Case (applySub sub e) vs (applySub sub as)
    applySub _ t@Type{} = t
instance Substitutable a b c => Substitutable a b (Alt c) where
    applySub sub (Alt c x) = Alt c (applySub sub x)