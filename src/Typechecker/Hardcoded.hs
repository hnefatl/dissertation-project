module Typechecker.Hardcoded where

import qualified Data.Map as M
import qualified Data.Set as S

import Names
import Typechecker.Types
import Typechecker.Typeclasses

builtinConstructors :: M.Map VariableName QuantifiedType
builtinConstructors = M.fromList
    [ (VariableName "True", Quantified S.empty $ Qualified S.empty typeBool),
      (VariableName "False", Quantified S.empty $ Qualified S.empty typeBool)
    ]

builtinFunctions :: M.Map VariableName QuantifiedType
builtinFunctions = M.fromList
    [
        let t = makeFun [ta, ta] ta
        in (VariableName "+", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance classNum ta)) t)
    ,
        let t = makeFun [ta, ta] ta
        in (VariableName "-", Quantified (S.singleton a) $ Qualified (S.singleton (IsInstance classNum ta)) t)
    ,
        let t = makeFun [ta, ta] typeBool
        in (VariableName "==", Quantified (S.singleton a) $ Qualified (S.singleton $ IsInstance classEq ta) t)
    ,
        (VariableName "&&", Quantified S.empty $ Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
        (VariableName "||", Quantified S.empty $ Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
        (VariableName "not", Quantified S.empty $ Qualified S.empty (makeFun [typeBool] typeBool))
    ]
    where a = TypeVariable (TypeVariableName "a") KindStar
          ta = TypeVar a
          classEq = TypeConstantName "Eq"
          classNum = TypeConstantName "Num"

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
        a = TypeVar (TypeVariable (TypeVariableName "a") KindStar)
        b = TypeVar (TypeVariable (TypeVariableName "b") KindStar)
        classShow = TypeConstantName "Show"
        classEq = TypeConstantName "Eq"
        classNum = TypeConstantName "Num"
        classFractional = TypeConstantName "Fractional"