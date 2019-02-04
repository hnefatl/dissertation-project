import Distribution.Simple (defaultMain)
import System.Process (callCommand)

main :: IO ()
main = do
    -- TODO(kc506): Remove -g to disable debugging output
    callCommand "javac -g -classpath runtime runtime/*.java"
    defaultMain