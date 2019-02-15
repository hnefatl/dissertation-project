module Tuples where

import BasicPrelude hiding (replicate)
import Data.Text    (replicate)

makeTupleName :: Int -> Text
makeTupleName elems = "(" <> replicate (elems - 1) "," <> ")"
