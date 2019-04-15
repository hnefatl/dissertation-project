{-# LANGUAGE TemplateHaskell #-}

module StdLibEmbed where

import BasicPrelude   (IsString)
import Data.FileEmbed (embedStringFile)

stdLibContents :: IsString s => s
stdLibContents = $(embedStringFile "StdLib.hs")
