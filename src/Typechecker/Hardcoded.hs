module Typechecker.Hardcoded where

import           BasicPrelude
import qualified Data.Map                as M
import qualified Data.Set                as S

import           Names
import           Typechecker.Typeclasses
import           Typechecker.Types

builtinConstructors :: M.Map VariableName QuantifiedType
builtinConstructors = M.fromList
    [ ("True", Quantified S.empty $ Qualified S.empty typeBool)
    , ("False", Quantified S.empty $ Qualified S.empty typeBool)
    , ("Nothing", Quantified (S.singleton a) $ Qualified S.empty maybeA)
    , ("Just", Quantified (S.singleton a) $ Qualified S.empty $ makeFunUnsafe [ta] maybeA)
    , ("(,)", Quantified (S.fromList [a, b]) $ Qualified S.empty $ makeFunUnsafe [ta, tb] $ makeTupleUnsafe [ta, tb])
    , ("(,,)", Quantified (S.fromList [a, b, c]) $ Qualified S.empty $ makeFunUnsafe [ta, tb, tc] $ makeTupleUnsafe [ta, tb, tc])
    , (":", Quantified (S.fromList [a]) $ Qualified S.empty $ makeFunUnsafe [ta, makeListUnsafe ta] $ makeListUnsafe ta)
    , ("[]", Quantified (S.fromList [a]) $ Qualified S.empty $ makeListUnsafe ta) ]
    where a = TypeVariable "a" KindStar
          b = TypeVariable "b" KindStar
          c = TypeVariable "c" KindStar
          ta = TypeVar a
          tb = TypeVar b
          tc = TypeVar c
          maybeT = TypeCon $ TypeConstant "Maybe" (KindFun KindStar KindStar)
          maybeA = applyTypeFunUnsafe maybeT ta


builtinKinds :: M.Map TypeVariableName Kind
builtinKinds = M.fromList
    [ ("Bool", KindStar)
    , ("Int", KindStar)
    , ("Maybe", KindFun KindStar KindStar)
    , ("[]", KindFun KindStar KindStar)
    , ("(,)", KindFun (KindFun KindStar KindStar) KindStar)
    , ("(,,)", KindFun (KindFun (KindFun KindStar KindStar) KindStar) KindStar)
    -- Dev cheat: after deoverloading the typeclasses become type constants, add them here rather than in an extra
    -- deoverloading step (which will be required in the end)
    , ("Num", KindFun KindStar KindStar) ]


builtinFunctions :: M.Map VariableName QuantifiedType
builtinFunctions = M.fromList
    [
        let t = makeFunUnsafe [ta, ta] ta
        in ("+", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance "Num" ta)) t)
    ,
        let t = makeFunUnsafe [ta, ta] ta
        in ("-", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance "Num" ta)) t)
    ,
        let t = makeFunUnsafe [ta, ta] typeBool
        in ("==", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance "Eq" ta) t)
    ,
        ("&&", Quantified S.empty $ Qualified S.empty (makeFunUnsafe [typeBool, typeBool] typeBool))
    ,
        ("||", Quantified S.empty $ Qualified S.empty (makeFunUnsafe [typeBool, typeBool] typeBool))
    ,
        ("not", Quantified S.empty $ Qualified S.empty (makeFunUnsafe [typeBool] typeBool))
    ]
    where a = TypeVariable "a" KindStar
          ta = TypeVar a

builtinClasses :: ClassEnvironment
builtinClasses = M.fromList
    [
        ("Eq", Class S.empty $ S.fromList
            [
                Qualified S.empty (IsInstance "Eq" typeBool),
                Qualified S.empty (IsInstance "Eq" typeInt),
                Qualified S.empty (IsInstance "Eq" typeChar),
                Qualified (S.fromList [IsInstance "Eq" a]) (IsInstance "Eq" $ makeListUnsafe a),
                Qualified (S.fromList [IsInstance "Eq" a, IsInstance "Eq" b]) (IsInstance "Eq" $ makeTupleUnsafe [a, b])
            ]
        ),
        ("Num", Class (S.singleton "Eq") $ S.fromList
            [
                Qualified S.empty (IsInstance "Num" typeInt)
            ]
        ),
        ("Fractional", Class (S.singleton "Num") $ S.fromList
            [
                Qualified S.empty (IsInstance "Fractional" typeFloat)
            ]
        ),
        ("Show", Class S.empty $ S.fromList
            [
                Qualified S.empty (IsInstance "Show" typeBool),
                Qualified S.empty (IsInstance "Show" typeInt),
                Qualified S.empty (IsInstance "Show" typeChar),
                Qualified (S.singleton $ IsInstance "Show" a) (IsInstance "Show" $ makeListUnsafe a),
                Qualified (S.fromList [IsInstance "Show" a, IsInstance "Show" b]) (IsInstance "Show" $ makeTupleUnsafe [a, b])
            ])
    ]
    where
        a = TypeVar (TypeVariable "a" KindStar)
        b = TypeVar (TypeVariable "b" KindStar)

builtinDictionaries :: M.Map TypePredicate VariableName
builtinDictionaries = M.fromList
    [ (IsInstance "Num" typeInt, "dNumInt")
    , (IsInstance "Eq" typeInt, "dEqInt")
    , (IsInstance "Eq" typeBool, "dEqBool") ]
