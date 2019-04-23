module Tuples where

import BasicPrelude hiding (replicate)
import Data.Text    (replicate, unpack)
import Text.Regex.TDFA

makeTupleName :: Int -> Text
makeTupleName elems = "(" <> replicate (elems - 1) "," <> ")"

isTupleName :: Text -> Bool
isTupleName = (=~ pat) . unpack
    where pat = "\\(,+\\)" :: String