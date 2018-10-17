module Typechecker.Hardcoded where

import qualified Data.Map as M
import qualified Data.Set as S

import Typechecker.Types
import Typechecker.Typeclasses

builtinConstructors :: M.Map Id UninstantiatedQualifiedType
builtinConstructors = M.fromList
    [ ("True", Qualified S.empty typeBool),
      ("False", Qualified S.empty typeBool),
      let v = TypeVar (TypeDummy "a" KindStar)
          t = makeFun v (makeFun v v)
      in ("+", Qualified (S.singleton (IsInstance "Num" v)) t)
    ]

builtinClasses :: ClassEnvironment
builtinClasses = M.fromList
    [
        ("Eq", Class S.empty $ S.fromList [Qualified S.empty (IsInstance "Eq" typeInt)])
    ]