import Test.Tasty

import AlphaEqSpec
import Preprocessor.RenamerSpec
import Preprocessor.DependencySpec
import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypeTaggerSpec
import Typechecker.TypeclassesSpec
import Typechecker.TypecheckerSpec
import Backend.DeoverloadSpec
import Backend.ILASpec

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