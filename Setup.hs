import Distribution.Simple (defaultMain)
import System.Process (callCommand)

main :: IO ()
main = do
    callCommand "javac -classpath runtime runtime/*.java"
    defaultMain