import Test.Tasty

import AlphaEqSpec
import Backend.DeoverloadSpec
import Backend.ILASpec
import BasicPrelude
import Preprocessor.DependencySpec
import Preprocessor.RenamerSpec
import Typechecker.SubstitutionSpec
import Typechecker.TypecheckerSpec
import Typechecker.TypeclassesSpec
import Typechecker.TypeTaggerSpec
import Typechecker.UnifierSpec

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
            , Backend.ILASpec.test ]
    ]

main :: IO ()
main = defaultMain tests
