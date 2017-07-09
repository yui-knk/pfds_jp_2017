import Test.Tasty
import Test.Tasty.HUnit

import LeftistHeap


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "" $
      rank2 E @?= 0
  , testCase "" $
      rank2 (T 1 'x' E E) @?= 1
  ]
