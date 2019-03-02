module Backend.DeoverloadSpec where

import           Test.Tasty              (TestTree, testGroup)
import           Test.Tasty.HUnit        (assertFailure, testCase)

import           Language.Haskell.Syntax
import           Language.Haskell.Parser

import           BasicPrelude            hiding (intercalate)
import           Control.Monad.Except    (runExcept)
import           Data.Text               (intercalate, unpack)
import           Data.Functor.Identity   (runIdentity)
import           Data.Tuple.Extra        (both)

import           AlphaEq
import           Preprocessor.Info       (getClassInfo)
import           Backend.Deoverload
import           ExtraDefs               (pretty, synPrint)
import           Logger                  (runLoggerT)
import           NameGenerator
import           Typechecker.Hardcoded
import           Typechecker.Typechecker
import           SyntaxTraversals (expTraverse)

unpackEither :: Either e a -> (e -> Text) -> IO a
unpackEither (Left err) f = assertFailure $ unpack (f err)
unpackEither (Right x) _  = return x

makeTest :: Text -> Text -> TestTree
makeTest sActual sExpected = testCase (unpack sActual) $ case both (parseModule . unpack) (sExpected, sActual) of
    (ParseOk expected, ParseOk actualModule) -> do
        let infer = runTypeInferrer (inferModuleWithBuiltins actualModule)
            (((eTiOutput, tState), inferLogs), i) = runNameGenerator (runLoggerT infer) 0
        (taggedModule, ts) <- unpackEither (runExcept eTiOutput) id
        let moduleClassInfo = getClassInfo taggedModule
            deoverload = runDeoverload (deoverloadModule moduleClassInfo taggedModule) ts builtinKinds builtinClasses
            ((eDeoverloaded, dState), deoverloadLogs) = evalNameGenerator (runLoggerT deoverload) i
            expected' = listCorrector $ stripParens expected
        actual <- unpackEither (runExcept eDeoverloaded) (\err -> unlines [err, "Expected:", synPrint expected', "Got:", synPrint taggedModule, pretty tState, pretty dState, unlines inferLogs, unlines deoverloadLogs])
        let result = runExcept $ alphaEqError expected' actual
        unpackEither result (\err -> unlines [err, "Expected:", synPrint expected', "Got:", synPrint actual, pretty tState, pretty dState, unlines inferLogs, unlines deoverloadLogs])
    (ParseFailed _ _, _) -> assertFailure "Failed to parse expected"
    (_, ParseFailed _ _) -> assertFailure "Failed to parse actual"

-- Create text representing a haskell expression creating a list
makeList :: [Text] -> Text -> Text
makeList es t = foldr (\x y -> "((((:) :: " <> t <> " -> [" <> t <> "] -> [" <> t <> "]) (" <> x  <> " :: a)" <> " :: [" <> t <> "] -> [" <> t <> "]) " <> y <> " :: [" <> t <> "])") ("(([]) :: [" <> t <> "])") es

-- We parse `[]` as an empty list comprehension (a `HsList` node) when we want it to be parsed as a constructor `[]` as
-- that's what we convert it to internally
listCorrector :: HsModule -> HsModule
listCorrector = runIdentity . expTraverse f
    where
        f (HsList []) = pure $ HsCon $ Special $ HsListCon
        f e = pure e

test :: TestTree
test = testGroup "Deoverload"
    [ makeTest
        "f = \\x -> x + x"
        "f = (\\d -> (\\x -> ((((+) :: Num a -> a -> a -> a) (d :: Num a) :: a -> a -> a) (x :: a) :: a -> a) (x :: a) :: a) :: a -> a) :: Num a -> a -> a"
    , makeTest
        "x = if True then 0 else 1"
        "x = (\\d -> (if True :: Bool then 0 :: a else 1 :: a) :: a) :: Num a -> a"
    , makeTest
        "x = [1]"
        ("x = (\\d -> " <> makeList ["1"] "a" <> ") :: Num a -> [a]")
    , makeTest
        "x = (\\y -> [0, y])"
        ("x = (\\d -> (\\y -> " <> makeList ["0", "y"] "a" <> ") :: a -> [a]) :: Num a -> a -> [a]")
    , makeTest
        "(x, y) = (True, [1])"
        ("(x, y) = (\\d -> ((((,) :: Bool -> [a] -> (Bool, [a])) (True :: Bool) :: [a] -> (Bool, [a])) " <> makeList ["1"] "a" <> ") :: (Bool, [a])) :: Num a -> (Bool, [a])")
    , makeTest
        "f = \\x -> x ; y = f 0"
        "f = (\\x -> x :: a) :: a -> a ; y = (\\d -> (f :: b -> b) (0 :: b) :: b) :: Num b -> b"
    , makeTest
        "f = \\x -> x ; y = f True"
        "f = (\\x -> x :: a) :: a -> a ; y = (f :: Bool -> Bool) (True :: Bool) :: Bool"
    , makeTest
        "f = \\x -> x + x ; y = f (0 :: Int)"
        "f = (\\d -> (\\x -> ((((+) :: Num a -> a -> a -> a) (d :: Num a) :: a -> a -> a) (x :: a) :: a -> a) (x :: a) :: a) :: a -> a) :: Num a -> a -> a ; y = ((f :: Num Int -> Int -> Int) (dNumInt :: Num Int) :: Int -> Int) (((0 :: Int) :: Int) :: Int) :: Int"
    , let
        input = intercalate " ; "
            [ "const = \\x _ -> x"
            , "f = \\y z -> const (y == y) (z + z)"
            , "g = f True (1 :: Int)" ]
        -- Subexpressions of the expected expression
        eq = "((==) :: Eq c -> c -> c -> Bool) (dc :: Eq c) :: c -> c -> Bool"
        plus = "((+) :: Num d -> d -> d -> d) (dd :: Num d) :: d -> d -> d"
        yeqy = "((" <> eq <> ") (y :: c) :: c -> Bool) (y :: c) :: Bool"
        zplusz = "((" <> plus <> ") (z :: d) :: d -> d) (z :: d) :: d"
        constapp = "((const :: Bool -> d -> Bool) (" <> yeqy <> ") :: d -> Bool) (" <> zplusz <> ") :: Bool"
        bbody = "(\\y z -> (" <> constapp <> ")) :: c -> d -> Bool"
        bdicts = "(\\dc dd -> (" <> bbody <> ")) :: Eq c -> Num d -> c -> d -> Bool"
        expected = unlines
            [ "const = (\\x _ -> x :: a) :: a -> b -> a"
            , "f = " <> bdicts
            , "g = ((((f :: Eq Bool -> Num Int -> Bool -> Int -> Bool) (dEqBool :: Eq Bool) :: Num Int -> Bool -> Int -> Bool) (dNumInt :: Num Int) :: Bool -> Int -> Bool) (True :: Bool) :: Int -> Bool) (((1 :: Int) :: Int) :: Int) :: Bool"
            ]
      in makeTest input expected
    ]
