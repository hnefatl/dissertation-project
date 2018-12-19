import Test.Tasty

import AlphaEqSpec
import Preprocessor.RenamerSpec
import Preprocessor.DependencySpec
import Typechecker.SubstitutionSpec
import Typechecker.UnifierSpec
import Typechecker.TypecheckerSpec
import Typechecker.TypeTaggerSpec
import Typechecker.TypeclassesSpec

tests :: TestTree
tests = testGroup "Tests"
    [ AlphaEqSpec.test
    , Preprocessor.RenamerSpec.test
    , Preprocessor.DependencySpec.test
    , Typechecker.SubstitutionSpec.test
    , Typechecker.UnifierSpec.test
    , Typechecker.TypecheckerSpec.test
    , Typechecker.TypeTaggerSpec.test
    , Typechecker.TypeclassesSpec.test
    ]

main :: IO ()
main = defaultMain tests