{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, TupleSections #-}

-- |Intermediate Language A - basically GHC's Core but without support for complicated language features like GADTs.
module Backend.ILA where

import BasicPrelude hiding (head, exp)
import TextShow (TextShow, showb, showt)
import TextShow.Instances ()
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local, reader, asks)
import Control.Monad.Except (MonadError, ExceptT, throwError)
import TextShowHsSrc ()
import Language.Haskell.Syntax
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as Text
import Data.List (foldl', intersperse)
import Data.Default (Default, def)

import Names
import ExtraDefs (middleText)
import Logger
import NameGenerator
import Typechecker.Types (Type(..), Qualified(..), Quantified(..), QuantifiedSimpleType, Kind)
import qualified Typechecker.Types as T
import Preprocessor.ContainedNames

-- |A literal value
data Literal = LiteralInt Integer
             | LiteralFrac Rational
             | LiteralChar Char
             | LiteralString String
    deriving (Eq, Ord)
instance TextShow Literal where
    showb (LiteralInt i) = showb i <> " :: Int"
    showb (LiteralFrac r) = showb r <> " :: Rational"
    showb (LiteralChar c) = showb c <> " :: Char"
    showb (LiteralString s) = showb s <> " :: String"

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt = Alt AltConstructor [VariableName] Expr
    deriving (Eq, Ord)
instance TextShow Alt where
    showb (Alt con vs e) = showb con <> (if null vs then "" else " ") <> args <> " -> " <> showb e
        where args = mconcat $ intersperse " " $ map showb vs
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName | LitCon Literal | Default
    deriving (Eq, Ord)
instance TextShow AltConstructor where
    showb (DataCon n) = showb n
    showb (LitCon l) = showb l
    showb Default = "default"

data Expr = Var VariableName Type -- Variable/function/data constructor
          | Lit Literal
          | App Expr Expr -- Application of terms or types
          | Lam VariableName Type Expr -- Abstraction of terms or types
          | Let VariableName Type Expr Expr
          | Case Expr [VariableName] [Alt] -- in `case e of [x] { a1 ; a2 ; ... }`, x is bound to the value of e.
          | Type Type
    deriving (Eq, Ord)
instance TextShow Expr where
    showb (Var n t) = showb n <> " :: " <> showb t
    showb (Lit l) = showb l
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (Lam v t b) = "λ(" <> showb v <> " :: " <> showb t <> ") -> " <> showb b
    showb (Let v t e1 e2) = "let " <> showb v <> " :: " <> showb t <> " = " <> showb e1 <> " in " <> showb e2
    showb (Case s bs as) = "case " <> showb s <> " of " <> showb bs <> " { " <> cases <> " }" 
        where cases = mconcat $ intersperse " ; " $ map showb as
    showb (Type t) = "Type " <> showb t


-- |A recursive/nonrecursive binding of a Core expression to a name.
data Binding = NonRec VariableName Expr | Rec (M.Map VariableName Expr)
    deriving (Eq, Ord)
instance TextShow Binding where
    showb (NonRec v e) = "NonRec: " <> showb v <> " = " <> showb e
    showb (Rec m) = mconcat $ intersperse "\n" $ headline:bodylines
        where (v1, e1):bs = M.toList m
              headline =    "Rec: " <> showb v1 <> " = " <> showb e1
              bodylines = [ "     " <> showb v  <> " = " <> showb e | (v, e) <- bs ]

data ConverterState = ConverterState
    { types :: M.Map VariableName QuantifiedSimpleType
    , renamings :: M.Map VariableName VariableName
    , kinds :: M.Map TypeVariableName Kind }
instance Default ConverterState where
    def = ConverterState
        { types = M.empty
        , renamings = M.empty
        , kinds = M.empty }

newtype Converter a = Converter (ReaderT ConverterState (ExceptT Text (LoggerT NameGenerator)) a)
    deriving (Functor, Applicative, Monad, MonadReader ConverterState, MonadError Text, MonadLogger, MonadNameGenerator)

evalConverter :: Converter a -> M.Map VariableName QuantifiedSimpleType -> M.Map TypeVariableName Kind -> ExceptT Text (LoggerT NameGenerator) a
evalConverter (Converter inner) ts ks = runReaderT (local (addTypes ts) $ local (addKinds ks) inner) def

addRenaming :: VariableName -> VariableName -> ConverterState -> ConverterState
addRenaming x = addRenamings . M.singleton x
addRenamings :: M.Map VariableName VariableName -> ConverterState -> ConverterState
addRenamings rens state = state { renamings = M.union rens (renamings state) }
addTypes :: M.Map VariableName QuantifiedSimpleType -> ConverterState -> ConverterState
addTypes ts state = state { types = M.union ts (types state) }
addKinds :: M.Map TypeVariableName Kind -> ConverterState -> ConverterState
addKinds ks state = state { kinds = M.union ks (kinds state) }
getKinds :: Converter (M.Map TypeVariableName Kind)
getKinds = asks kinds

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

-- |Construct an expression representing a tuple given the expressions and types of each element
makeTuple :: [(Expr, Type)] -> Expr
makeTuple ps = makeTuple' es t
    where (es, ts) = unzip ps
          t = T.makeFun ts (T.makeTuple ts)
-- |Construct an expression representing a tuple given the expressions of each element and the type of the tuple
makeTuple' :: [Expr] -> Type -> Expr
makeTuple' es t = foldl' App base es
    where base = Var (VariableName "(,)") t

makeList :: MonadError Text m => [(Expr, Type)] -> m Expr
makeList [] = throwError "Empty list passed to makeList"
makeList ps = do
    let (es, ts) = unzip ps
        uniqueTypes = S.fromList ts
    case S.toList uniqueTypes of
        [t] -> return $ makeList' es t
        uts -> throwError $ "Mismatching types passed to makeList: " <> showt uts
makeList' :: [Expr] -> Type -> Expr
makeList' es t = foldr (\x y -> App (App cons x) y) nil es
    where cons = Var (VariableName ":") (T.makeFun [t, T.makeList t] $ T.makeList t)
          nil = Var (VariableName "[]") (T.makeList t)

makeError :: Type -> Expr
makeError = Var (VariableName "error")

getPatRenamings :: HsPat -> Converter ([VariableName], M.Map VariableName VariableName)
getPatRenamings pat = do
    boundNames <- S.toAscList <$> getPatContainedNames pat
    renames <- M.fromList <$> mapM (\n -> (n,) <$> freshVarName) boundNames
    return (boundNames, renames)

getPatVariableTypes :: MonadError Text m => HsPat -> Type -> m (M.Map VariableName Type)
getPatVariableTypes (HsPVar v) t = return $ M.singleton (convertName v) t
getPatVariableTypes (HsPLit _) _ = return $ M.empty
getPatVariableTypes (HsPApp _ ps) t = do
    (argTypes, _) <- T.unmakeFun t
    unless (length ps /= length argTypes) $ throwError "Partially applied data constructor in ILA lowering"
    M.unions <$> zipWithM getPatVariableTypes ps argTypes
getPatVariableTypes (HsPTuple ps) t = do
    argTypes <- T.unmakeTuple t
    unless (length ps /= length argTypes) $ throwError "Partially applied tuple in ILA lowering"
    M.unions <$> zipWithM getPatVariableTypes ps argTypes
getPatVariableTypes (HsPList ps) t = do
    elementType <- T.unmakeList t
    M.unions <$> mapM (flip getPatVariableTypes $ elementType) ps
getPatVariableTypes (HsPParen p) t = getPatVariableTypes p t
getPatVariableTypes (HsPAsPat n p) t = M.insert (convertName n) t <$> getPatVariableTypes p t
getPatVariableTypes HsPWildCard _ = return M.empty
getPatVariableTypes _ _ = throwError "Unsupported pattern"

-- TODO(kc506): Enforce that case expressions can only have variable names as their heads, then it's easy to restrict
-- them to be thunks rather than whole subcomputations?
-- ^ Think this is ANF, can wait
toIla :: HsModule -> Converter [Binding]
toIla (HsModule _ _ _ _ decls) = concat <$> mapM declToIla decls

declToIla :: HsDecl -> Converter [Binding]
declToIla (HsPatBind _ pat rhs _) = do
    -- Precompute a mapping from the bound names in the patterns to some fresh names
    (boundNames, renames) <- getPatRenamings pat
    let boundNameLength = length boundNames
    boundTypes <- mapM getSimpleType boundNames -- Type of each bound name
    writeLog $ "Found bound names: " <> Text.intercalate " " (zipWith (middleText " :: ") boundNames boundTypes)
    -- Create expressions for each of the fresh names
    let renamedExps = zipWith (\name t -> Var (renames M.! name) t) boundNames boundTypes
    -- Generate an expression that matches the patterns then returns a tuple of every variable
    let resultType = T.makeTuple boundTypes
        resultTuple = makeTuple' renamedExps resultType
    ts <- M.fromList <$> mapM (\n -> (n,) <$> getType n) boundNames
    resultExpr <- local (addRenamings renames . addTypes ts) $ do
        rhsExpr <- rhsToIla rhs -- Get an expression for the RHS using the renamings from actual name to temporary name
        rhst <- rhsType rhs
        patToIla pat rhst rhsExpr resultTuple
    -- The variable name used to store the result tuple: each bound name in the patterns pulls their values from this
    resultName <- freshVarName
    -- For each bound name, generate a binding that extracts the variable from the result tuple
    let extractorMap (name, index) = do
            bindingType <- getSimpleType name -- Get the type of this bound variable
            tempNames <- replicateM boundNameLength freshVarName -- Create temporary variables for pattern matching on
            let tempName = tempNames !! index -- Get the temporary name in the right position in the tuple
                body = Var tempName bindingType
            -- Just retrieve the specific output variable
            return $ NonRec name $ Case (Var resultName resultType) []
                [ Alt (DataCon $ VariableName "(,)") tempNames body
                , Alt Default [] (makeError bindingType) ]
    extractors <- mapM extractorMap (zip boundNames [0..])
    return $ Rec (M.singleton resultName resultExpr):extractors
declToIla d = throwError $ "Unsupported declaration\n" <> showt d

rhsType :: HsRhs -> Converter Type
rhsType (HsUnGuardedRhs (HsExpTypeSig _ _ t)) = do
    ks <- getKinds
    Qualified _ t' <- T.synToQualType ks t
    return t'
rhsType (HsUnGuardedRhs _) = throwError "Missing explicit type sig on top-level expression in RHS"
rhsType (HsGuardedRhss _) = throwError "Guarded RHS not supported"
rhsToIla :: HsRhs -> Converter Expr
rhsToIla (HsUnGuardedRhs e) = expToIla e
rhsToIla (HsGuardedRhss _) = throwError "Guarded RHS not supported"

expToIla :: HsExp -> Converter Expr
expToIla (HsVar v) = do
    name <- getRenamedOrDefault (convertName v)
    Var name <$> getSimpleType name
expToIla (HsCon c) = Var name <$> getSimpleType name
    where name = convertName c
expToIla (HsLit l) = Lit <$> litToIla l
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla HsInfixApp{} = throwError "Infix applications not supported: should've been removed by the typechecker"
expToIla (HsLambda _ [] e) = expToIla e
expToIla (HsExpTypeSig l (HsLambda l' (p:pats) e) t) = do
    ks <- getKinds
    argName <- freshVarName
    Qualified _ expType <- T.synToQualType ks t
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
        patToIla p argType (Var argName argType) nextBody
    return (Lam argName argType body)
expToIla HsLambda{} = throwError "Lambda with body not wrapped in explicit type signature"
expToIla (HsLet _ _) = throwError "Let not yet supported"
expToIla (HsIf cond e1 e2) = do
    condExp <- expToIla cond
    e1Exp <- expToIla e1
    e2Exp <- expToIla e2
    let alts = [Alt (DataCon $ VariableName "True") [] e1Exp, Alt (DataCon $ VariableName "False") [] e2Exp]
    return $ Case condExp [] alts
expToIla (HsCase _ _) = throwError "Urgh case not yet supported"
expToIla (HsExpTypeSig _ (HsTuple exps) t) = do
    ks <- getKinds
    es <- mapM expToIla exps
    Qualified _ t' <- T.synToQualType ks t
    return $ makeTuple' es t'
expToIla (HsExpTypeSig _ (HsList exps) t) = do
    ks <- getKinds
    es <- mapM expToIla exps
    Qualified _ t' <- T.synToQualType ks t
    return $ makeList' es t'
expToIla (HsParen exp) = expToIla exp
expToIla (HsExpTypeSig _ e _) = expToIla e
expToIla e = throwError $ "Unsupported expression: " <> showt e


litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c) = return $ LiteralChar c
litToIla (HsString s) = return $ LiteralString s
litToIla (HsInt i) = return $ LiteralInt i  -- Replace with fromInteger to cast to arbitrary Num?
litToIla (HsFrac r) = return $ LiteralFrac r
litToIla l = throwError $ "Unboxed primitives not supported: " <> showt l

-- |Lowers a pattern match on a given expression with a given body expression into the core equivalent
-- We convert a pattern match on a variable into a case statement that binds the variable to the head and always
-- defaults to the body.
-- We convert a constructor application by recursively converting the sub-pattern-matches then chaining them with
-- matching this data constructor.
-- The result is a massive mess of case statements. They are composable though, which is useful for building larger
-- expressions, and we can prune redundant/overly complicated substructures in an optimisation pass if necessary.
patToIla :: HsPat -> Type -> Expr -> Expr -> Converter Expr
patToIla (HsPVar n) _ head body = do
    renamedVar <- getRenamed $ convertName n
    return $ Case head [renamedVar] [Alt Default [] body]
patToIla (HsPLit l) _ head body = throwError "Need to figure out dictionary passing before literals"
patToIla (HsPApp con args) _ head body = do
    argNames <- replicateM (length args) freshVarName
    (argTypes, expType) <- T.unmakeCon <$> getSimpleType (convertName con)
    let argExpTypes = zipWith3 (\p n t -> (p, Var n t, t)) args argNames argTypes
    body' <- foldM (\body' (pat, head', t) -> patToIla pat t head' body') body argExpTypes
    return $ Case head [] [Alt (DataCon $ convertName con) argNames body', Alt Default [] $ makeError expType]
patToIla (HsPTuple pats) t head body = patToIla (HsPApp (UnQual $ HsIdent "(,)") pats) t head body
patToIla (HsPList []) t head body = return $ Case head [] [Alt nilCon [] body, Alt Default [] $ makeError t]
    where nilCon = DataCon $ VariableName "[]"
patToIla (HsPList (p:ps)) listType head body = do
    elementType <- T.unmakeList listType
    headName <- freshVarName
    tailName <- freshVarName
    headExpr <- patToIla p elementType (Var headName elementType) body
    tailExpr <- patToIla (HsPList ps) listType (Var tailName listType) headExpr
    return $ Case head []
        [ Alt (DataCon $ VariableName ":") [headName, tailName] tailExpr
        , Alt Default [] $ makeError listType ]
patToIla (HsPParen pat) t head body = patToIla pat t head body
patToIla (HsPAsPat name pat) t head body = do
    expr <- patToIla pat t head body
    asArgName <- getRenamed (convertName name)
    case expr of
        Case head' captures alts' -> return $ Case head' (asArgName:captures) alts'
        _ -> throwError "@ pattern binding non-case translation"
patToIla HsPWildCard _ head body = return $ Case head [] [Alt Default [] body]
patToIla p _ _ _ = throwError $ "Unsupported pattern: " <> showt p