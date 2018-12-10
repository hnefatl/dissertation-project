{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts, TupleSections #-}

module Backend.ILA where

import Prelude hiding (head)
import Control.Monad.Reader
import Control.Monad.Except
import Language.Haskell.Syntax
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl', intercalate)
import Text.Printf

import Names
import NameGenerator
import Typechecker.Types (Type, QuantifiedType)
import Preprocessor.ContainedNames

-- |A literal value
data Literal = LiteralInt Integer
             | LiteralFrac Rational
             | LiteralChar Char
             | LiteralString String
    deriving (Eq, Ord, Show)

-- |An alternative in a case expression.
-- Consists of a constructor, a list of the variables bound to its arguments, and an RHS
-- If there's a literal or nested data constructor then it needs to be bound to a variable
-- and checked subsequently, as the alternatives can only contain variable names.
data Alt = Alt AltConstructor [VariableName] Expr
    deriving (Eq, Ord)
instance Show Alt where
    show (Alt con vs e) = printf "%s %s -> %s" (show con) (unwords $ map show vs) (show e)
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon VariableName | LitCon Literal | Default
    deriving (Eq, Ord)
instance Show AltConstructor where
    show (DataCon n) = show n
    show (LitCon l) = show l
    show Default = "__default"

data Expr = Var VariableName -- Variable/function/data constructor
          | Lit Literal
          | App Expr Expr -- Term application
          | Lam VariableName Expr -- Term abstraction
          | Let VariableName Expr Expr
          | Case Expr [VariableName] [Alt] -- case e of x { a1 ; a2 ; ... }. x is bound to the value of e.
          | Type Type
    deriving (Eq, Ord)
instance Show Expr where
    show (Var n) = show n
    show (Lit l) = show l
    show (App e1 e2) = printf "(%s) (%s)" (show e1) (show e2) 
    show (Lam v b) = printf "λ%s -> %s" (show v) (show b)
    show (Let v e1 e2) = printf "let %s = %s in %s" (show v) (show e1) (show e2)
    show (Case s bs as) = printf "case %s of %s { %s }" (show s) (show bs) (intercalate " ; " $ map show as)
    show (Type t) = "Type " ++ show t


-- |A recursive/nonrecursive binding of a Core expression to a name.
data Binding = NonRec VariableName Expr | Rec (M.Map VariableName Expr)
    deriving (Eq, Ord)
instance Show Binding where
    show (NonRec v e) = printf "NonRec: %s = %s" (show v) (show e)
    show (Rec m) = unlines (headline:bodylines)
        where (v1, e1):bs = M.toList m
              headline =    printf "Rec: %s = %s" (show v1) (show e1)
              bodylines = [ printf "     %s = %s" (show v) (show e) | (v, e) <- bs ]

data ConverterState = ConverterState
    { types :: M.Map VariableName QuantifiedType
    , renamings :: M.Map VariableName VariableName }

newtype Converter a = Converter (ReaderT ConverterState (ExceptT String NameGenerator) a)
    deriving (Functor, Applicative, Monad, MonadReader ConverterState, MonadError String, MonadNameGenerator VariableName)

runConverter :: MonadError String m => Converter a -> M.Map VariableName QuantifiedType -> NameGenerator (m a)
runConverter (Converter inner) ts = liftEither <$> runExceptT (runReaderT inner initState)
    where initState = ConverterState { types = ts, renamings = M.empty }

addRenaming :: VariableName -> VariableName -> ConverterState -> ConverterState
addRenaming x = addRenamings . M.singleton x
addRenamings :: M.Map VariableName VariableName -> ConverterState -> ConverterState
addRenamings rens state = state { renamings = M.union rens (renamings state) }

getRenamed :: VariableName -> Converter VariableName
getRenamed name = reader (M.lookup name . renamings) >>= \case
    Nothing -> throwError $ "Variable " ++ show name ++ " not in renamings."
    Just renamed -> return renamed

getRenamedOrDefault :: VariableName -> Converter VariableName
getRenamedOrDefault name = reader (M.findWithDefault name name . renamings)

getType :: VariableName -> Converter QuantifiedType
getType name = reader (M.lookup name . types) >>= \case
    Nothing -> throwError $ "Variable " ++ show name ++ " not in type environment"
    Just t -> return t

makeTuple :: [Expr] -> Expr
makeTuple = foldl' App (Var $ VariableName "(,)")

makeList :: [Expr] -> Expr
makeList = foldr (\x y -> App (App (Var $ VariableName ":") x) y) (Var $ VariableName "[]")

makeError :: Expr
makeError = Var $ VariableName "error"

toIla :: HsModule -> Converter [Binding]
toIla (HsModule _ _ _ _ decls) = concat <$> mapM declToIla decls

declToIla :: HsDecl -> Converter [Binding]
declToIla (HsPatBind _ pat rhs _) = do
    resultName <- freshName
    rhsExpr <- rhsToIla rhs
    boundNames <- S.toAscList <$> getPatContainedNames pat
    let boundNameLength = length boundNames
    -- Generate renamings for each bound variable, so we don't get variable name conflicts
    renamings <- M.fromList <$> forM boundNames (\n -> (n,) <$> freshName)
    -- Generate an expression that matches the patterns then returns a tuple of every variable
    let resultTuple = makeTuple $ map (Var . (M.!) renamings) boundNames
    resultExpr <- local (addRenamings renamings) (patToIla pat rhsExpr resultTuple)
    -- For each bound name, generate a binding that extracts the variable from the tuple
    let extractorMap (name, index) = do
            tempNames <- replicateM boundNameLength freshName
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
expToIla (HsVar v) = Var <$> getRenamed (convertName v)
expToIla (HsCon c) = return $ Var (convertName c)
expToIla (HsLit l) = Lit <$> litToIla l
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla (HsInfixApp _ _ _) = error "Infix applications not supported"
expToIla (HsLambda _ [p] e) = do
    argName <- freshName
    body <- patToIla p (Var argName) =<< expToIla e
    return (Lam argName body)
expToIla (HsLambda l (p:pats) e) = do
    argName <- freshName
    body <- patToIla p (Var argName) =<< expToIla (HsLambda l pats e)
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
    argNames <- replicateM (length args) freshName
    let argNamePairs = zipWith (\p n -> (p, Var n)) args argNames
    body' <- foldM (flip $ uncurry patToIla) body argNamePairs
    return $ Case head [] [Alt (DataCon $ convertName con) argNames body', Alt Default [] makeError]
patToIla (HsPTuple pats) head body = patToIla (HsPApp (UnQual $ HsIdent "(,)") pats) head body
patToIla (HsPList []) head body = do
    return $ Case head [] [Alt (DataCon $ VariableName "[]") [] body, Alt Default [] makeError]
patToIla (HsPList (p:ps)) head body = do
    headName <- freshName
    tailName <- freshName
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