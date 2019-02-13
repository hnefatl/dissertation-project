{-# Language QuasiQuotes #-}

module WholeProgram where

import           Test.Tasty
import           Test.Tasty.HUnit

import           BasicPrelude
import           NeatInterpolation
import           Control.Monad.Extra     (whenJust)
import qualified Data.Set                as S
import           Data.Text               (pack, unpack)
import           TextShow                (TextShow, showt)
import           System.Process          (readCreateProcess, shell)
import           System.IO.Temp


--makeTest :: Text -> Text -> TestTree
--makeTest source expected = do
--    tempDir <- getCanonicalTemporaryDirectory
--    withTempDirectory tempDir "compiler-test" $ \dir -> do
--        sourceFile <- writeTempFile dir "compiler-test" source
--        let cmd = shell $ "stack exec compiler-exe " <> sourceFile
--        undefined
--    -- Run compile
--    -- Package to jar?
--    -- Save source to tmp file
--    -- Exec jar
--    --readCreateProcess 
--
--test :: TestTree
--test = testGroup "Whole Program"
--    [
--        (
--            [text|
--                _main = True
--            |]
--        ,
--            "Data { branch = 1, data = { } }"
--        )
--    ]