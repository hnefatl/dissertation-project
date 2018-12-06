import Test.Tasty

import Preprocessor.RenamerSpec
import Preprocessor.DependencySpec
import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypecheckerSpec
import Typechecker.TypeclassesSpec
import Backend.ILASpec

tests :: TestTree
tests = testGroup "Tests"
    [   testGroup "Preprocessor"
            [ Preprocessor.RenamerSpec.test
            , Preprocessor.DependencySpec.test ]
    ,
        testGroup "Typechecker"
            [ Typechecker.SubstitutionSpec.test
            , Typechecker.UnifierSpec.test
            , Typechecker.TypecheckerSpec.test
            , Typechecker.TypeclassesSpec.test ]
    ,
        testGroup "Backend"
            [ Backend.ILASpec.test ]
    ]

main :: IO ()
main = defaultMain tests