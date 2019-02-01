import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple (buildHook, UserHooks, defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.Setup (BuildFlags)
import System.Process (callCommand)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { buildHook = customBuildHook }

customBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuildHook desc lbi uh bf = do
    (buildHook simpleUserHooks) desc lbi uh bf
    callCommand "javac -classpath runtime runtime/*.java"