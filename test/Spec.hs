import           Test.Tasty

import qualified AlphaEqSpec
import qualified Backend.DeoverloadSpec
import qualified Backend.ILAANFSpec
import qualified Backend.ILASpec
import           BasicPrelude
import qualified Preprocessor.DependencySpec
import qualified Preprocessor.RenamerSpec
import qualified Typechecker.SubstitutionSpec
import qualified Typechecker.TypecheckerSpec
import qualified Typechecker.TypeclassesSpec
import qualified Typechecker.TypeTaggerSpec
import qualified Typechecker.UnifierSpec
import qualified WholeProgram

tests :: TestTree
tests = testGroup "Tests"
    [   testGroup "Utilities"
        [ AlphaEqSpec.test ]
    ,
        testGroup "Preprocessor"
            [ Preprocessor.RenamerSpec.test
            , Preprocessor.DependencySpec.test ]
    ,
        testGroup "Typechecker"
            [ Typechecker.SubstitutionSpec.test
            , Typechecker.UnifierSpec.test
            , Typechecker.TypeclassesSpec.test
            , Typechecker.TypecheckerSpec.test
            , Typechecker.TypeTaggerSpec.test ]
    ,
        testGroup "Backend"
            [ Backend.DeoverloadSpec.test
            , Backend.ILASpec.test
            , Backend.ILAANFSpec.test ]
    , WholeProgram.test
    ]

main :: IO ()
main = defaultMain tests
