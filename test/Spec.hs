import Test.Tasty

import Typechecker.SubstitutionsSpec

tests :: TestTree
tests = testGroup "Tests"
    [ Typechecker.SubstitutionsSpec.test
    ]

main :: IO ()
main = defaultMain tests
