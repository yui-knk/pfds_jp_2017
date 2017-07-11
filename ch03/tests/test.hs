import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import qualified LeftistHeapTest as LHTest
import qualified WeightBiasedLeftistHeapTest as WBLHTest
import qualified BinomialHeapTest as BHTest
import qualified BinomialHeap2Test as BH2Test

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
    [ LHTest.unitTestsForInterfaces
    , LHTest.unitTestsForHelpers
    , WBLHTest.unitTestsForInterfaces
    , WBLHTest.unitTestsForHelpers
    , BHTest.unitTestsForInterfaces
    , BH2Test.unitTestsForInterfaces
    ]
