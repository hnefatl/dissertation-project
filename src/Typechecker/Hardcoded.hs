module Typechecker.Hardcoded where

import qualified Data.Map as M
import qualified Data.Set as S

import ExtraDefs
import Typechecker.Types
import Typechecker.Typeclasses

builtinConstructors :: M.Map Id QuantifiedType
builtinConstructors = M.fromList
    [ (Id "True", Quantified S.empty $ Qualified S.empty typeBool),
      (Id "False", Quantified S.empty $ Qualified S.empty typeBool)
    ]

builtinFunctions :: M.Map Id QuantifiedType
builtinFunctions = M.fromList
    [
        let t = makeFun [ta, ta] ta
        in (Id "+", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance (Id "Num") ta)) t)
    ,
        let t = makeFun [ta, ta] ta
        in (Id "-", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance (Id "Num") ta)) t)
    ,
        let t = makeFun [ta, ta] typeBool
        in (Id "==", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance (Id "Eq") ta) t)
    ,
        (Id "&&", Quantified S.empty $ Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
        (Id "||", Quantified S.empty $ Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
        (Id "not", Quantified S.empty $ Qualified S.empty (makeFun [typeBool] typeBool))
    ]
    where a = TypeVariable (Id "a") KindStar
          ta = TypeVar a

builtinClasses :: ClassEnvironment
builtinClasses = M.fromList
    [
        (classEq, Class S.empty $ S.fromList
            [
                Qualified S.empty (IsInstance classEq typeBool),
                Qualified S.empty (IsInstance classEq typeInt),
                Qualified S.empty (IsInstance classEq typeChar),
                Qualified (S.fromList [IsInstance classEq a]) (IsInstance classEq $ makeList a),
                Qualified (S.fromList [IsInstance classEq a, IsInstance classEq b]) (IsInstance classEq $ makeTuple [a, b])
            ]
        ),
        (classNum, Class (S.singleton classEq) $ S.fromList
            [
                Qualified S.empty (IsInstance classNum typeInt)
            ]
        ),
        (classFractional, Class (S.singleton classNum) $ S.fromList
            [
                Qualified S.empty (IsInstance classFractional typeFloat)
            ]
        ),
        (classShow, Class S.empty $ S.fromList 
            [
                Qualified S.empty (IsInstance classShow typeBool),
                Qualified S.empty (IsInstance classShow typeInt),
                Qualified S.empty (IsInstance classShow typeChar),
                Qualified (S.singleton $ IsInstance classShow a) (IsInstance classShow $ makeList a),
                Qualified (S.fromList [IsInstance classShow a, IsInstance classShow b]) (IsInstance classShow $ makeTuple [a, b])
            ])
    ]
    where
        a = TypeVar (TypeVariable (Id "a") KindStar)
        b = TypeVar (TypeVariable (Id "b") KindStar)
        classShow = Id "Show"
        classEq = Id "Eq"
        classNum = Id "Num"
        classFractional = Id "Fractional"