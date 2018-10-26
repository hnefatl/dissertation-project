module Typechecker.Hardcoded where

import qualified Data.Map as M
import qualified Data.Set as S

import Typechecker.Types
import Typechecker.Typeclasses

builtinConstructors :: M.Map Id UninstantiatedQualifiedType
builtinConstructors = M.fromList
    [ ("True", Qualified S.empty typeBool),
      ("False", Qualified S.empty typeBool)
    ]

builtinFunctions :: M.Map Id UninstantiatedQualifiedType
builtinFunctions = M.fromList
    [
      let v = TypeVar (TypeDummy "a" KindStar)
          t = makeFun [v, v] v
      in ("+", Qualified (S.singleton (IsInstance "Num" v)) t)
    ,
      ("&&", Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
      ("||", Qualified S.empty (makeFun [typeBool, typeBool] typeBool))
    ,
      ("not", Qualified S.empty (makeFun [typeBool] typeBool))
    ]

builtinClasses :: ClassEnvironment
builtinClasses = M.fromList
    [
        ("Eq", Class S.empty $ S.fromList [Qualified S.empty (IsInstance "Eq" typeInt)]),
        ("Num", Class (S.singleton "Eq") $ S.fromList [Qualified S.empty (IsInstance "Num" typeInt)]),
        ("Fractional", Class (S.singleton "Num") $ S.fromList [Qualified S.empty (IsInstance "Fractional" typeFloat)]),
        ("Show", Class S.empty $ S.fromList 
            [
                Qualified S.empty (IsInstance "Show" typeInt)
            ,
                let t = TypeVar (TypeDummy "a" KindStar) in 
                Qualified (S.singleton $ IsInstance "Show" t) (IsInstance "Show" $ makeList t)
            ])
    ]