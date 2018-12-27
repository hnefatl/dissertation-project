{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, TupleSections #-}

-- |Intermediate Language A - basically GHC's Core but without support for complicated language features like GADTs.
module Backend.ILA where

import BasicPrelude
import TextShow (TextShow, showb, showt)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, local, reader)
import Control.Monad.Except (MonadError, ExceptT, throwError)
import Language.Haskell.Syntax
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl', intersperse)
import Data.Default (Default, def)

import Names
import NameGenerator
import Typechecker.Types (Type, QuantifiedType)
import Preprocessor.ContainedNames

-- |A literal value
data Literal = LiteralInt Integer
             | LiteralFrac Rational
             | LiteralChar Char
             | LiteralString String
    deriving (Eq, Ord)
instance TextShow Literal where
    showb (LiteralInt i) = showb i
    showb (LiteralFrac r) = showb r
    showb (LiteralChar c) = showb c
    showb (LiteralString s) = showb s

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt = Alt AltConstructor [VariableName] Expr
    deriving (Eq, Ord)
instance TextShow Alt where
    showb (Alt con vs e) = showb con <> " " <> args <> " -> " <> showb e
        where args = mconcat $ intersperse " " $ map showb vs
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName | LitCon Literal | Default
    deriving (Eq, Ord)
instance TextShow AltConstructor where
    showb (DataCon n) = showb n
    showb (LitCon l) = showb l
    showb Default = "default"

data Expr = Var VariableName -- Variable/function/data constructor
          | Lit Literal
          | App Expr Expr -- Term application
          | Lam VariableName Expr -- Term abstraction
          | Let VariableName Expr Expr
          | Case Expr [VariableName] [Alt] -- case e of x { a1 ; a2 ; ... }. x is bound to the value of e.
          | Type Type
    deriving (Eq, Ord)
instance TextShow Expr where
    showb (Var n) = showb n
    showb (Lit l) = showb l
    showb (App e1 e2) = "(" <> showb e1 <> ") (" <> showb e2 <> ")"
    showb (Lam v b) = "λ" <> showb v <> " -> " <> showb b
    showb (Let v e1 e2) = "let " <> showb v <> " = " <> showb e1 <> " in " <> showb e2
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
    { types :: M.Map VariableName QuantifiedType
    , renamings :: M.Map VariableName VariableName }
instance Default ConverterState where
    def = ConverterState
        { types = M.empty
        , renamings = M.empty }

newtype Converter a = Converter (ReaderT ConverterState (ExceptT Text NameGenerator) a)
    deriving (Functor, Applicative, Monad, MonadReader ConverterState, MonadError Text, MonadNameGenerator)

evalConverter :: Converter a -> M.Map VariableName QuantifiedType -> ExceptT Text NameGenerator a
evalConverter (Converter inner) ts = runReaderT (local (addTypes ts) inner) def

addRenaming :: VariableName -> VariableName -> ConverterState -> ConverterState
addRenaming x = addRenamings . M.singleton x
addRenamings :: M.Map VariableName VariableName -> ConverterState -> ConverterState
addRenamings rens state = state { renamings = M.union rens (renamings state) }
addTypes :: M.Map VariableName QuantifiedType -> ConverterState -> ConverterState
addTypes ts state = state { types = M.union ts (types state) }

getRenamed :: VariableName -> Converter VariableName
getRenamed name = reader (M.lookup name . renamings) >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not in renamings."
    Just renamed -> return renamed

getRenamedOrDefault :: VariableName -> Converter VariableName
getRenamedOrDefault name = reader (M.findWithDefault name name . renamings)

getType :: VariableName -> Converter QuantifiedType
getType name = reader (M.lookup name . types) >>= \case
    Nothing -> throwError $ "Variable " <> showt name <> " not in type environment"
    Just t -> return t

makeTuple :: [Expr] -> Expr
makeTuple = foldl' App (Var $ VariableName "(,)")

makeList :: [Expr] -> Expr
makeList = foldr (\x y -> App (App (Var $ VariableName ":") x) y) (Var $ VariableName "[]")

makeError :: Expr
makeError = Var $ VariableName "error"

getPatRenamings :: HsPat -> Converter ([VariableName], M.Map VariableName VariableName)
getPatRenamings pat = do
    boundNames <- S.toAscList <$> getPatContainedNames pat
    renames <- M.fromList <$> mapM (\n -> (n,) <$> freshVarName) boundNames
    return (boundNames, renames)

-- TODO(kc506): Enforce that case expressions can only have variable names as their heads, then it's easy to restrict
-- them to be thunks rather than whole subcomputations?
toIla :: HsModule -> Converter [Binding]
toIla (HsModule _ _ _ _ decls) = concat <$> mapM declToIla decls

declToIla :: HsDecl -> Converter [Binding]
declToIla (HsPatBind _ pat rhs _) = do
    (boundNames, renames) <- getPatRenamings pat
    let boundNameLength = length boundNames
    -- Generate an expression that matches the patterns then returns a tuple of every variable
    let resultTuple = makeTuple $ map (Var . (M.!) renames) boundNames
    resultExpr <- local (addRenamings renames) $ do
        rhsExpr <- rhsToIla rhs
        patToIla pat rhsExpr resultTuple
    -- For each bound name, generate a binding that extracts the variable from the tuple
    resultName <- freshVarName
    let extractorMap (name, index) = do
            tempNames <- replicateM boundNameLength freshVarName
            let body = Var (tempNames !! index) -- Just retrieve the specific output variable
            return $ NonRec name (Case (Var resultName) [] [Alt (DataCon $ VariableName "(,)") tempNames body, Alt Default [] makeError])
    extractors <- mapM extractorMap (zip boundNames [0..])
    return $ Rec (M.singleton resultName resultExpr):extractors
declToIla (HsFunBind matches) = undefined
declToIla _ = error "Unsupported declaration"

rhsToIla :: HsRhs -> Converter Expr
rhsToIla (HsUnGuardedRhs e) = expToIla e
rhsToIla (HsGuardedRhss _) = error "Guarded RHS not supported"

expToIla :: HsExp -> Converter Expr
expToIla (HsVar v) = Var <$> getRenamedOrDefault (convertName v)
expToIla (HsCon c) = return $ Var (convertName c)
expToIla (HsLit l) = Lit <$> litToIla l
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla (HsInfixApp _ _ _) = error "Infix applications not supported"
expToIla (HsLambda l [] e) = throwError "Lambda without arguments"
expToIla (HsLambda l (p:pats) e) = do
    argName <- freshVarName
    (_, renames) <- getPatRenamings p
    body <- local (addRenamings renames) $ do
        nextBody <- case pats of
            [] -> expToIla e
            pats' -> expToIla (HsLambda l pats' e)
        patToIla p (Var argName) nextBody
    return (Lam argName body)
expToIla (HsLet decls e) = error "Let not yet supported"
expToIla (HsIf cond e1 e2) = do
    condExp <- expToIla cond
    e1Exp <- expToIla e1
    e2Exp <- expToIla e2
    let alts = [Alt (DataCon $ VariableName "True") [] e1Exp, Alt (DataCon $ VariableName "False") [] e2Exp]
    return $ Case condExp [] alts
expToIla (HsCase e alts) = error "Urgh case not yet supported"
expToIla (HsTuple exps) = makeTuple <$> mapM expToIla exps
expToIla (HsList exps) = makeList <$> mapM expToIla exps
expToIla (HsParen exp) = expToIla exp
expToIla _ = error "Unsupported expression"


litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c) = return $ LiteralChar c
litToIla (HsString s) = return $ LiteralString s
litToIla (HsInt i) = return $ LiteralInt i  -- Replace with fromInteger to cast to arbitrary Num?
litToIla (HsFrac r) = return $ LiteralFrac r
litToIla _ = error "Unboxed primitives not supported"

-- |Lowers a pattern match on a given expression with a given body expression into the core equivalent
-- We convert a pattern match on a variable into a case statement that binds the variable to the head and always
-- defaults to the body.
-- We convert a constructor application by recursively converting the sub-pattern-matches then chaining them with
-- matching this data constructor.
-- The result is a massive mess of case statements. They are composable though, which is useful for building larger
-- expressions, and we can prune redundant/overly complicated substructures in an optimisation pass if necessary.
patToIla :: HsPat -> Expr -> Expr -> Converter Expr
patToIla (HsPVar n) head body = do
    renamedVar <- getRenamed $ convertName n
    return $ Case head [renamedVar] [Alt Default [] body]
patToIla (HsPLit l) head body = error "Need to figure out dictionary passing before literals"
patToIla (HsPApp con args) head body = do
    argNames <- replicateM (length args) freshVarName
    let argNamePairs = zipWith (\p n -> (p, Var n)) args argNames
    body' <- foldM (flip $ uncurry patToIla) body argNamePairs
    return $ Case head [] [Alt (DataCon $ convertName con) argNames body', Alt Default [] makeError]
patToIla (HsPTuple pats) head body = patToIla (HsPApp (UnQual $ HsIdent "(,)") pats) head body
patToIla (HsPList []) head body = return $ Case head [] [Alt nilCon [] body, Alt Default [] makeError]
    where nilCon = DataCon $ VariableName "[]"
patToIla (HsPList (p:ps)) head body = do
    headName <- freshVarName
    tailName <- freshVarName
    headExpr <- patToIla p (Var headName) body
    tailExpr <- patToIla (HsPList ps) (Var tailName) headExpr
    return $ Case head [] [Alt (DataCon $ VariableName ":") [headName, tailName] tailExpr, Alt Default [] makeError]
patToIla (HsPParen pat) head body = patToIla pat head body
patToIla (HsPAsPat name pat) head body = do
    expr <- patToIla pat head body
    asArgName <- getRenamed (convertName name)
    case expr of
        Case head' captures alts' -> return $ Case head' (asArgName:captures) alts'
        _ -> error "@ pattern binding non-case translation"
patToIla HsPWildCard head body = return $ Case head [] [Alt Default [] body]
patToIla _ _ _ = error "Unsupported pattern"