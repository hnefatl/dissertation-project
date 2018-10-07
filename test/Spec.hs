import Test.Tasty

import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec

tests :: TestTree
tests = testGroup "Tests"
    [ Typechecker.SubstitutionSpec.test
    , Typechecker.UnifierSpec.test
    ]

main :: IO ()
main = defaultMain tests
