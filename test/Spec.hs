import Test.Tasty

import Typechecker.SubstitutionSpec

tests :: TestTree
tests = testGroup "Tests"
    [ Typechecker.SubstitutionSpec.test
    ]

main :: IO ()
main = defaultMain tests
