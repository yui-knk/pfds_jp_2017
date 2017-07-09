import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import qualified LeftistHeapTest as LHTest
import qualified WeightBiasedLeftistHeapTest as WBLHTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ LHTest.unitTestsForInterfaces
    , LHTest.unitTestsForHelpers
    , WBLHTest.unitTestsForInterfaces
    , WBLHTest.unitTestsForHelpers
    ]
