module Main where

import BasicPrelude
import Data.Default        (def)
import Options.Applicative
import System.FilePath     (takeBaseName)

import Compiler            (Flags(..), compile)

main :: IO ()
main = do
    flags <- parseCommandLine
    case inputFiles flags of
        [f] -> compile flags f
        _   -> putStrLn "Currently only supports a single input file"

parseCommandLine :: IO Flags
parseCommandLine = do
    flags <- execParser $ info (helper <*> parseFlags) (fullDesc <> progDesc "A Compiler from Haskell to Java Bytecode")
    return $ case package flags of
        -- Default value for package is the filename of the first input file
        "" -> flags { package = takeBaseName $ head $ inputFiles flags }
        _  -> flags

parseFlags :: Parser Flags
parseFlags = Flags
    <$> switch
        ( long "verbose"
       <> short 'v'
       <> help "Display (very) verbose logging output"
       <> showDefault)
    <*> switch
        ( long "let-lift"
       <> short 'l'
       <> help "Perform the let-lifting optimisation"
       <> showDefault)
    <*> switch
        ( long "dedupe"
       <> short 't'
       <> help "Perform the binding deduplication optimisation"
       <> showDefault)
    <*> switch
        ( long "unreachable-code-elimination"
       <> short 'u'
       <> help "Perform the unreachable code elimination optimisation"
       <> showDefault)
    <*> switch
        ( long "no-std-import"
       <> short 'n'
       <> help "Don't import the standard library"
       <> showDefault)
    <*> switch
        ( long "dump-types"
       <> help "Display all types after compiling"
       <> showDefault)
    <*> strOption
        ( long "build-dir"
       <> short 'd'
       <> help "Directory to store build files"
       <> value (buildDir def)
       <> showDefault)
    <*> strOption
        ( long "output-jar"
       <> short 'o'
       <> help "Output jar path"
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
    <*> strOption
        ( long "package"
       <> short 'p'
       <> help "Java package used for the output java files"
       <> value "")
    <*> some (argument str (metavar "input-files" <> help "Haskell source files to compile"))
