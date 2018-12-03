{-# Language GeneralizedNewtypeDeriving, LambdaCase, FlexibleContexts #-}

module Backend.ILA where

import Control.Monad.Reader
import Control.Monad.Except
import Language.Haskell.Syntax
import qualified Data.Map as M

import Names
import NameGenerator
import Typechecker.Types

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
          | Case Expr (Maybe VariableName) [Alt] -- case e of x { a1 ; a2 ; ... }. x is bound to the value of e.
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

toIla :: HsModule -> Converter [Binding]
toIla = undefined

declToIla :: HsDecl -> Converter [Binding]
declToIla (HsPatBind _ pat rhs _) = undefined
declToIla (HsFunBind matches) = undefined

rhsToIla :: HsRhs -> Converter [Expr]
rhsToIla (HsUnGuardedRhs e) = pure <$> expToIla e
rhsToIla (HsGuardedRhss _) = error "Guarded RHS not supported"

expToIla :: HsExp -> Converter Expr
expToIla (HsVar v) = return $ Var (convertName v)
expToIla (HsCon c) = return $ Var (convertName c)
expToIla (HsLit l) = Lit <$> litToIla l
expToIla (HsApp e1 e2) = App <$> expToIla e1 <*> expToIla e2
expToIla (HsInfixApp _ _ _) = error "Infix applications not supported"
expToIla (HsLambda _ pats e) = undefined

litToIla :: HsLiteral -> Converter Literal
litToIla (HsChar c) = return $ LiteralChar c
litToIla (HsString s) = return $ LiteralString s
litToIla (HsInt i) = return $ LiteralInt i  -- Replace with fromInteger to cast to arbitrary Num?
litToIla (HsFrac r) = return $ LiteralFrac r
litToIla _ = error "Unboxed primitives not supported"

-- |Lowers a pattern match on a given expression with a given body expression into the core equivalent
patToIla :: HsPat -> Expr -> Expr -> Converter Expr
patToIla (HsPVar n) scrut body = return $ Var (convertName n)
patToIla (HsPLit l) scrut body = Lit <$> litToIla l
patToIla (HsPApp con args) scrut body = do
    argNames <- replicateM (length args) freshName
    body' <- 
    return $ Case scrut Nothing [Alt (DataCon $ convertName con) argNames undefined, Alt Default [] undefined]

patsToIla :: [HsPat] -> Expr -> Expr -> Converter Expr
patsToIla [] _ body = body
patsToIla 