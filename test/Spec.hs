import Test.Tasty

import AlphaEqSpec
import Preprocessor.RenamerSpec
import Preprocessor.DependencySpec
import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypeclassesSpec
import Typechecker.TypecheckerSpec
import Backend.ILASpec
import Backend.DeoverloadSpec

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
            , Typechecker.TypecheckerSpec.test ]
    ,
        testGroup "Backend"
            [ Backend.ILASpec.test
            , Backend.DeoverloadSpec.test ]
    ]

main :: IO ()
main = defaultMain tests