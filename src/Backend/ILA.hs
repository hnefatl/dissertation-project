{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}

-- |Intermediate Language A - basically GHC's Core but without support for complicated language features like GADTs.
module Backend.ILA where

import           BasicPrelude                hiding (exp, head)
import           Control.Monad.Except        (ExceptT, MonadError, throwError)
import           Control.Monad.Reader        (MonadReader, ReaderT, local, reader, runReaderT)
import           Control.Monad.State.Strict  (MonadState, StateT, gets, modify, runStateT)
import           Data.Default                (Default, def)
import           Data.List                   (foldl', intersperse)
import qualified Data.Map.Strict             as M
import qualified Data.Set                    as S
import qualified Data.Text                   as Text
import           Language.Haskell.Syntax
import           TextShow                    (TextShow, showb, showt)
import           TextShow.Instances          ()
import           TextShowHsSrc               ()

import           ExtraDefs                   (mapError, middleText, synPrint)
import           Logger
import           NameGenerator
import           Names
import           Preprocessor.ContainedNames (HasBoundVariables, HasFreeVariables, getBoundVariables, getFreeVariables)
import           Typechecker.Types           (Kind(..), Qualified(..), Quantified(..), QuantifiedSimpleType, Type(..),
                                              TypePredicate(..), TypeVariable(..))
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
    , branches   :: [(VariableName, [(Type, Strictness)])] }
    deriving (Eq, Ord, Show)
instance TextShow Datatype where
    showb = fromString . show

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
    deriving (Eq, Ord)
instance TextShow Literal where
    showb (LiteralInt i)    = showb i <> " :: Int"
    showb (LiteralFrac r)   = showb r <> " :: Rational"
    showb (LiteralChar c)   = showb c <> " :: Char"
    showb (LiteralString s) = showb s <> " :: String"

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt a = Alt AltConstructor [VariableName] a
    deriving (Eq, Ord)
instance TextShow a => TextShow (Alt a) where
    showb (Alt con vs e) = showb con <> (if null vs then "" else " ") <> args <> " -> " <> showb e
        where args = mconcat $ intersperse " " $ map showb vs
getAltConstructor :: Alt a -> AltConstructor
getAltConstructor (Alt c _ _) = c
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName | LitCon Literal | Default
    deriving (Eq, Ord)
instance TextShow AltConstructor where
    showb (DataCon n) = showb n
    showb (LitCon l)  = showb l
    showb Default     = "default"
isDefaultAlt :: Alt a -> Bool
isDefaultAlt (Alt Default _ _) = True
isDefaultAlt _                 = False
isDataAlt :: Alt a -> Bool
isDataAlt (Alt (DataCon _) _ _) = True
isDataAlt _                     = False
isLiteralAlt :: Alt a -> Bool
isLiteralAlt (Alt (LitCon _) _ _) = True
isLiteralAlt _                    = False

-- |A recursive/nonrecursive binding of a Core expression to a name.
data Binding a = NonRec VariableName a | Rec (M.Map VariableName a)
    deriving (Eq, Ord)
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
data Expr = Var VariableName Type -- Variable/function/data constructor
          | Lit Literal Type
          | App Expr Expr -- Application of terms or types
          | Lam VariableName Type Expr -- Abstraction of terms or types
          | Let VariableName Type Expr Expr
          | Case Expr [VariableName] [Alt Expr] -- in `case e of [x] { a1 ; a2 ; ... }`, x is bound to e.
          | Type Type
    deriving (Eq, Ord)
instance TextShow Expr where
    showb (Var n t) = showb n <> " :: " <> showb t
    showb (Lit l t) = showb l <> " :: " <> showb t
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> cases <> " }"
        where cases = mconcat $ intersperse " ; " $ map showb as
    showb (Type t) = "Type " <> showb t

getExprType :: MonadError Text m => Expr -> m Type
getExprType (Var _ t)                = return t
getExprType (Lit _ t)                = return t
getExprType (App e1 _)               = snd <$> (T.unwrapFun =<< getExprType e1)
getExprType (Lam _ t e)              = T.makeFun [t] <$> getExprType e
getExprType (Let _ _ _ e)            = getExprType e
getExprType (Case _ _ [])            = throwError "No alts in case"
getExprType (Case _ _ (Alt _ _ e:_)) = getExprType e
getExprType (Type t)                 = return t

data ConverterReadableState = ConverterReadableState
    { types     :: M.Map VariableName QuantifiedSimpleType
    , renamings :: M.Map VariableName VariableName }
    deriving (Eq, Show)
instance Default ConverterReadableState where
    def = ConverterReadableState
        { types = M.empty
        , renamings = M.empty }
instance TextShow ConverterReadableState where
    showb = fromString . show
data ConverterState = ConverterState
    { datatypes :: M.Map TypeVariableName Datatype
    , kinds     :: M.Map TypeVariableName Kind }
    deriving (Eq, Show)
instance Default ConverterState where
    def = ConverterState
        { datatypes = M.empty
        , kinds = M.empty }
instance TextShow ConverterState where
    showb = fromString . show

newtype Converter a = Converter (ReaderT ConverterReadableState (StateT ConverterState (ExceptT Text (LoggerT NameGenerator))) a)
    deriving (Functor, Applicative, Monad, MonadReader ConverterReadableState, MonadState ConverterState, MonadError Text, MonadLogger, MonadNameGenerator)

runConverter :: Converter a -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> ExceptT Text (LoggerT NameGenerator) (a, ConverterState)
runConverter x ts ks = runStateT (runReaderT (local (addTypes ts) inner) def) def
    where Converter inner = addKinds ks >> x
evalConverter :: Converter a -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> ExceptT Text (LoggerT NameGenerator) a
evalConverter x ts ks = fst <$> runConverter x ts ks

addRenaming :: VariableName -> VariableName -> ConverterReadableState -> ConverterReadableState
addRenaming x = addRenamings . M.singleton x
addRenamings :: M.Map VariableName VariableName -> ConverterReadableState -> ConverterReadableState
addRenamings rens state = state { renamings = M.union rens (renamings state) }
addTypes :: M.Map VariableName QuantifiedSimpleType -> ConverterReadableState -> ConverterReadableState
addTypes ts state = state { types = M.union ts (types state) }
addKinds :: M.Map TypeVariableName Kind -> Converter ()
addKinds ks = modify $ \st -> st { kinds = M.union ks (kinds st) }
getKinds :: Converter (M.Map TypeVariableName Kind)
getKinds = gets kinds
addDatatype :: Datatype -> Converter ()
addDatatype d = modify (\s -> s { datatypes = M.insert (typeName d) d (datatypes s) })

getRenamed :: VariableName -> Converter VariableName
getRenamed name = reader (M.lookup name . renamings) >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not in renamings."
    Just renamed -> return renamed

getRenamedOrDefault :: VariableName -> Converter VariableName
getRenamedOrDefault name = reader (M.findWithDefault name name . renamings)

getType :: VariableName -> Converter QuantifiedSimpleType
getType name = reader (M.lookup name . types) >>= \case
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

-- |Construct an expression representing a tuple given the expressions and types of each element
makeTuple :: [(Expr, Type)] -> Expr
makeTuple ps = makeTuple' es t
  where (es, ts) = unzip ps
        t        = T.makeFun ts (T.makeTuple ts)
-- |Construct an expression representing a tuple given the expressions of each element and the type of the tuple
makeTuple' :: [Expr] -> Type -> Expr
makeTuple' es t = foldl' App base es where base = Var "(,)" t

makeList :: MonadError Text m => [(Expr, Type)] -> m Expr
makeList [] = throwError "Empty list passed to makeList"
makeList ps = case S.toList uniqueTypes of
        [t] -> return $ makeList' es t
        uts -> throwError $ "Mismatching types passed to makeList: " <> showt uts
    where (es, ts)    = unzip ps
          uniqueTypes = S.fromList ts
makeList' :: [Expr] -> Type -> Expr
makeList' es t = foldr (\x y -> App (App cons x) y) nil es
    where cons = Var ":" (T.makeFun [t, T.makeList t] $ T.makeList t)
          nil  = Var "[]" (T.makeList t)

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
getPatVariableTypes (HsPTuple ps) t = do
    argTypes <- T.unmakeTuple t
    unless (length ps == length argTypes) $ throwError "Partially applied tuple in ILA lowering"
    M.unions <$> zipWithM getPatVariableTypes ps argTypes
getPatVariableTypes (HsPList ps) t = do
    elementType <- T.unmakeList t
    M.unions <$> mapM (flip getPatVariableTypes elementType) ps
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
        resultTuple = makeTuple (zip renamedExps boundTypes)
    ts <- M.fromList <$> mapM (\n -> (n, ) <$> getType n) boundNames
    resultExpr <- local (addRenamings renames . addTypes ts) $ do
        rhsExpr <- rhsToIla rhs -- Get an expression for the RHS using the renamings from actual name to temporary name
        rhst <- rhsType rhs
        patToIla pat rhst rhsExpr resultTuple resultType
    -- The variable name used to store the result tuple: each bound name in the patterns pulls their values from this
    resultName <- freshVarName
    -- For each bound name, generate a binding that extracts the variable from the result tuple
    let extractorMap (name, index) = do
            bindingType <- getSimpleType name -- Get the type of this bound variable
            tempNames   <- replicateM boundNameLength freshVarName -- Create temporary variables for pattern matching on
            let tempName = tempNames !! index -- Get the temporary name in the right position in the tuple
                body = Var tempName bindingType
            -- Just retrieve the specific output variable
            return $ NonRec name $ Case (Var resultName resultType) [] [ Alt (DataCon "(,)") tempNames body , Alt Default [] (makeError bindingType) ]
    extractors <- mapM extractorMap (zip boundNames [0 ..])
    return $ Rec (M.singleton resultName resultExpr):extractors
declToIla d@HsClassDecl{} = throwError $ "Class declaration should've been removed by the deoverloader:\n" <> synPrint d
declToIla d@(HsDataDecl _ ctx name args bs derivings) = case (ctx, derivings) of
    ([], []) -> do
        writeLog $ "Processing datatype " <> showt name
        bs' <- mapM conDeclToBranch bs
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
expToIla (HsExpTypeSig _ (HsVar v) t) = Var <$> getRenamedOrDefault (convertName v) <*> getSimpleFromSynType t
expToIla (HsExpTypeSig _ (HsCon c) t) = Var (convertName c) <$> getSimpleFromSynType t
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
    body <- local (addRenamings renames . addTypes renamedVariableTypes) $ do
        nextBody <- expToIla (HsExpTypeSig l (HsLambda l' pats e) reducedType)
        patToIla p argType (Var argName argType) nextBody bodyType
    return (Lam argName argType body)
expToIla HsLambda{} = throwError "Lambda with body not wrapped in explicit type signature"
expToIla (HsLet _ _) = throwError "Let not yet supported"
expToIla (HsIf cond e1 e2) = do
    condExp <- expToIla cond
    e1Exp   <- expToIla e1
    e2Exp   <- expToIla e2
    let alts = [ Alt (DataCon "True") [] e1Exp , Alt (DataCon "False") [] e2Exp ]
    return $ Case condExp [] alts
expToIla (HsCase _ _) = throwError "Urgh case not yet supported"
expToIla (HsTuple exps) = do
    es <- mapM expToIla exps
    ts <- mapM getExprType es
    return $ makeTuple $ zip es ts
expToIla (HsList exps) = do
    es <- mapM expToIla exps
    ts <- mapM getExprType es
    makeList $ zip es ts
expToIla (HsParen exp) = expToIla exp
expToIla (HsExpTypeSig _ e _) = expToIla e
expToIla e = throwError $ "Unsupported expression: " <> showt e


litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c)   = return $ LiteralChar c
litToIla (HsString s) = return $ LiteralString s
litToIla (HsInt i)    = return $ LiteralInt i  -- Replace with fromInteger to cast to arbitrary Num?
litToIla (HsFrac r)   = return $ LiteralFrac r
litToIla l            = throwError $ "Unboxed primitives not supported: " <> showt l

-- |Lowers a pattern match on a given expression with a given body expression into the core equivalent
-- We convert a pattern match on a variable into a case statement that binds the variable to the head and always
-- defaults to the body.
-- We convert a constructor application by recursively converting the sub-pattern-matches then chaining them with
-- matching this data constructor.
-- The result is a massive mess of case statements. They are composable though, which is useful for building larger
-- expressions, and we can prune redundant/overly complicated substructures in an optimisation pass if necessary.
patToIla :: HsPat -> Type -> Expr -> Expr -> Type -> Converter Expr
patToIla (HsPVar n) _ head body _ = do
    renamedVar <- getRenamed $ convertName n
    return $ Case head [renamedVar] [Alt Default [] body]
patToIla (HsPLit l) _ head body _ = throwError "Need to figure out dictionary passing before literals"
patToIla (HsPApp con args) t head body bodyType = do
    argNames <- replicateM (length args) freshVarName
    let (_, argTypes) = T.unmakeApp t
        argExpTypes = zipWith3 (\p n t' -> (p, Var n t', t')) args argNames argTypes
    body' <- foldM (\body' (pat, head', t') -> patToIla pat t' head' body' bodyType) body argExpTypes
    return $ Case head [] [ Alt (DataCon $ convertName con) argNames body', Alt Default [] $ makeError bodyType ]
patToIla (HsPTuple pats) t head body bodyType = patToIla (HsPApp (UnQual $ HsIdent "(,)") pats) t head body bodyType
patToIla (HsPList []) _ head body bodyType =
    return $ Case head [] [Alt nilCon [] body, Alt Default [] $ makeError bodyType ]
    where nilCon = DataCon "[]"
patToIla (HsPList (p:ps)) listType head body bodyType = do
    elementType <- T.unmakeList listType
    headName <- freshVarName
    tailName <- freshVarName
    headExpr <- patToIla p elementType (Var headName elementType) body bodyType
    tailExpr <- patToIla (HsPList ps) listType (Var tailName listType) headExpr bodyType
    return $ Case head [] [ Alt (DataCon ":") [headName, tailName] tailExpr , Alt Default [] $ makeError bodyType ]
patToIla (HsPParen pat) t head body bodyType = patToIla pat t head body bodyType
patToIla (HsPAsPat name pat) t head body bodyType = do
    expr <- patToIla pat t head body bodyType
    asArgName <- getRenamed (convertName name)
    case expr of
        Case head' captures alts' -> return $ Case head' (asArgName : captures) alts'
        _                         -> throwError "@ pattern binding non-case translation"
patToIla HsPWildCard _ head body _ = return $ Case head [] [Alt Default [] body]
patToIla p _ _ _ _ = throwError $ "Unsupported pattern: " <> showt p

instance HasFreeVariables a => HasFreeVariables (Alt a) where
    getFreeVariables (Alt _ vs e) = S.difference <$> getFreeVariables e <*> pure (S.fromList vs)
instance HasBoundVariables (Alt a) where
    getBoundVariables (Alt _ vs _) = return $ S.fromList vs
instance HasBoundVariables (Binding a) where
    getBoundVariables (NonRec v _) = return $ S.singleton v
    getBoundVariables (Rec m)      = return $ M.keysSet m
instance HasFreeVariables a => HasFreeVariables (Binding a) where
    getFreeVariables (NonRec v e) = S.delete v <$> getFreeVariables e
    getFreeVariables (Rec m)      = fmap S.unions $ forM (M.toList m) $ \(v, e) -> S.delete v <$> getFreeVariables e
