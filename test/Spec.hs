import Test.Tasty

import Preprocessor.RenamerSpec
import Preprocessor.DependencySpec
import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypecheckerSpec
import Typechecker.TypeclassesSpec

tests :: TestTree
tests = testGroup "Tests"
    [ Preprocessor.RenamerSpec.test
    , Preprocessor.DependencySpec.test
    , Typechecker.SubstitutionSpec.test
    , Typechecker.UnifierSpec.test
    , Typechecker.TypecheckerSpec.test
    , Typechecker.TypeclassesSpec.test
    ]

main :: IO ()
main = defaultMain tests