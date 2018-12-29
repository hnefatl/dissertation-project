module Main where

import BasicPrelude
import Data.Text               (pack, unpack)
import Language.Haskell.Parser (ParseMode(..), ParseResult(..), parseModuleWithMode)
import Language.Haskell.Syntax (SrcLoc(..))
import Text.Pretty.Simple
import TextShow                (showt)

main :: IO ()
main = do
    files <- getArgs
    forM_ files (compileFile . unpack)

printParseError :: SrcLoc -> Text -> IO ()
printParseError (SrcLoc file line col) msg =
    putStrLn $ "File \"" <> showt file <> "\", line " <> showt line <> ", column " <> showt col ++ ": " <> msg

compileFile :: FilePath -> IO ()
compileFile path = do
    contents <- readFile path
    let mode = ParseMode { parseFilename = path }
    case parseModuleWithMode mode (unpack contents) of
        ParseFailed loc msg -> printParseError loc (pack msg)
        ParseOk moduledef   -> pPrint moduledef
