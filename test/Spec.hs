import Test.Tasty

import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypecheckerSpec
import Typechecker.TypeclassesSpec

tests :: TestTree
tests = testGroup "Tests"
    [ Typechecker.SubstitutionSpec.test
    , Typechecker.UnifierSpec.test
    , Typechecker.TypecheckerSpec.test
    , Typechecker.TypeclassesSpec.test
    ]

main :: IO ()
main = defaultMain tests