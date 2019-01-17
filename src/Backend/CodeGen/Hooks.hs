module Backend.CodeGen.Hooks where

import Backend.CodeGen.Converter

-- A collection of extra generator actions, used to generate extra code in the main class. Allows injecting
-- compiler-defined code like `error`, `undefined`, and dictionaries for builtin instances like `Num Int`.

compilerGeneratedHooks :: [Converter ()]
compilerGeneratedHooks = []