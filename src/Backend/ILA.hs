{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts #-}

module Backend.ILA where

import Prelude hiding (head)
import Control.Monad.Reader
import Control.Monad.Except
import Language.Haskell.Syntax
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')

import Names
import NameGenerator
import Typechecker.Types (Type, QualifiedType)
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
    deriving (Eq, Ord, Show)
-- |A constructor that can be used in an alternative statement
data AltConstructor = DataCon ConstructorName | LitCon Literal | Default
    deriving (Eq, Ord, Show)

data Expr = Var VariableName -- Variable/function/data constructor
          | Lit Literal
          | App Expr Expr -- Term application
          | Lam VariableName Expr -- Term abstraction
          | Let VariableName Expr Expr
          | Case Expr [VariableName] [Alt] -- case e of x { a1 ; a2 ; ... }. x is bound to the value of e.
          | Type Type
    deriving (Eq, Ord, Show)


-- |A recursive/nonrecursive binding of a Core expression to a name.
data Binding = NonRec VariableName Expr | Rec (M.Map VariableName Expr)
    deriving (Eq, Ord, Show)


newtype Converter a = Converter (ReaderT (M.Map VariableName QualifiedType) (ExceptT String NameGenerator) a)
    deriving (Functor, Applicative, Monad, MonadReader (M.Map VariableName QualifiedType), MonadError String, MonadNameGenerator VariableName)

runConverter :: MonadError String m => Converter a -> M.Map VariableName QualifiedType -> NameGenerator (m a)
runConverter (Converter inner) s = liftEither <$> runExceptT (runReaderT inner s)

getType :: VariableName -> Converter QualifiedType
getType name = reader (M.lookup name) >>= \case
    Nothing -> throwError $ "Variable " ++ show name ++ " not in type environment"
    Just t -> return t

makeTuple :: [Expr] -> Expr
makeTuple = foldl' App (Var $ VariableName "(,)")

makeList :: [Expr] -> Expr
makeList = foldl' App (Var $ VariableName "[]")

toIla :: HsModule -> Converter [Binding]
toIla (HsModule _ _ _ _ decls) = concat <$> mapM declToIla decls

declToIla :: HsDecl -> Converter [Binding]
declToIla (HsPatBind _ pat rhs _) = do
    resultName <- freshName
    rhsExpr <- rhsToIla rhs
    boundNames <- S.toAscList <$> getPatContainedNames pat
    let boundNameLength = length boundNames
    -- Generate an expression that matches the patterns then returns a tuple of every variable
    let resultTuple = makeTuple (map Var boundNames)
    resultExpr <- patToIla pat rhsExpr resultTuple
    -- For each bound name, generate a binding that extracts the variable from the tuple
    let extractorMap (name, index) = do
            tempNames <- replicateM boundNameLength freshName
            let body = Var (tempNames !! index) -- Just retrieve the specific output variable
            return $ NonRec name (Case (Var resultName) [] [Alt (DataCon $ ConstructorName "(,)") tempNames body])
    extractors <- mapM extractorMap (zip boundNames [0..])
    return (NonRec resultName resultExpr:extractors)
declToIla (HsFunBind matches) = undefined
declToIla _ = error "Unsupported declaration"

rhsToIla :: HsRhs -> Converter Expr
rhsToIla (HsUnGuardedRhs e) = expToIla e
rhsToIla (HsGuardedRhss _) = error "Guarded RHS not supported"

expToIla :: HsExp -> Converter Expr
expToIla (HsVar v) = return $ Var (convertName v)
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
    let alts = [Alt (DataCon $ ConstructorName "True") [] e1Exp, Alt (DataCon $ ConstructorName "False") [] e2Exp]
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
patToIla :: HsPat -> Expr -> Expr -> Converter Expr
patToIla (HsPVar n) head body = return $ Case head [convertName n] [Alt Default [] body]
patToIla (HsPLit l) head body = error "Need to figure out dictionary passing before literals"
patToIla (HsPApp con args) head body = do
    argNames <- replicateM (length args) freshName
    let argNamePairs = zipWith (\p n -> (p, Var n)) args argNames
    body' <- foldM (flip $ uncurry patToIla) body argNamePairs
    return $ Case head [] [Alt (DataCon $ convertName con) argNames body', Alt Default [] undefined]
patToIla (HsPTuple pats) head body = patToIla (HsPApp (UnQual $ HsIdent "(,)") pats) head body
patToIla (HsPList pats) head body = patToIla (HsPApp (UnQual $ HsIdent "[]") pats) head body
patToIla (HsPParen pat) head body = patToIla pat head body
patToIla (HsPAsPat name pat) head body = do
    expr <- patToIla pat head body
    case expr of
        Case head' captures alts' -> return $ Case head' (convertName name:captures) alts'
        _ -> error "@ pattern binding non-case expression"
patToIla HsPWildCard head body = return $ Case head [] [Alt Default [] body]
patToIla _ _ _ = error "Unsupported pattern"