import Distribution.Simple (defaultMain)
import System.Process (callCommand)

main :: IO ()
main = do
    callCommand "javac -Xlint:all runtime/*.java"
    defaultMain