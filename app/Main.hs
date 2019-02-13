module Main where

import BasicPrelude
import Data.Default        (def)
import Options.Applicative

import Compiler            (Flags(..), compile)

main :: IO ()
main = do
    flags <- parseCommandLine
    case inputFiles flags of
        [f] -> compile flags f
        _   -> putStrLn "Currently only supports a single input file"

parseCommandLine :: IO Flags
parseCommandLine = execParser $
    info (helper <*> parseFlags) (fullDesc <> progDesc "A Compiler from Haskell to Java Bytecode")

parseFlags :: Parser Flags
parseFlags = Flags
    <$> switch
        ( long "verbose"
       <> short 'v'
       <> help "Display (very) verbose logging output"
       <> showDefault)
    <*> strOption
        ( long "output-dir"
       <> short 'd'
       <> help "Output directory for build files"
       <> value (outputDir def)
       <> showDefault)
    <*> strOption
        ( long "output-jar"
       <> short 'o'
       <> help "Output jar name"
       <> value (outputJar def)
       <> showDefault)
    <*> strOption
        ( long "output-class"
       <> short 'c'
       <> help "Output class name"
       <> value (outputClassName def)
       <> showDefault)
    <*> strOption
        ( long "runtime-file-dir"
       <> help "Directory containing the compiler's runtime class files."
       <> value (runtimeFileDir def)
       <> showDefault)
    <*> some (argument str (metavar "input-files" <> help "Haskell source files to compile"))
