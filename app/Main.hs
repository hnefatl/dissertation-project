module Main where

import Control.Monad
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Text.Pretty.Simple
import System.Environment

main :: IO ()
main = do
    files <- getArgs
    forM_ files compileFile

printParseError :: SrcLoc -> String -> IO ()
printParseError (SrcLoc file line col) msg =
    putStrLn $ "File \"" ++ file ++ "\", line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg 

compileFile :: FilePath -> IO ()
compileFile path = do
    contents <- readFile path
    let mode = ParseMode { parseFilename = path }
    case parseModuleWithMode mode contents of
        ParseFailed loc msg -> printParseError loc msg
        ParseOk moduledef -> pPrint moduledef