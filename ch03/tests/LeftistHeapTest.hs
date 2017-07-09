module LeftistHeapTest
( unitTestsForInterfaces
, unitTestsForHelpers
) where


import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import LeftistHeap


compare_heap_data :: Ord a => HeapData a -> HeapData a -> Bool
compare_heap_data E E = True
compare_heap_data E _ = False
compare_heap_data _ E = False
compare_heap_data h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
    (x == y) && (compare_heap_data a1 a2) && (compare_heap_data b1 b2)

unitTestsForInterfaces = testGroup "Unit tests"
  [ testCase "" $
    isEmpty (empty :: HeapData Char) @?= True

  , testCase "" $
      isEmpty (E :: HeapData Char) @?= True
  , testCase "" $
      isEmpty (T 1 'x' E E) @?= False

  , testCase "" $
      let act = merge E (E :: HeapData Char)
      in compare_heap_data act E @?= True
  , testCase "" $
      let act = merge (T 1 'x' E E) (E :: HeapData Char)
      in compare_heap_data act (T 1 'x' E E) @?= True
  , testCase "" $
      let act = merge (E :: HeapData Char) (T 1 'x' E E)
      in compare_heap_data act (T 1 'x' E E) @?= True
  , testCase "" $
      let act = merge (T 1 'x' E E) (T 1 'y' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True
  , testCase "" $
      let act = merge (T 1 'y' E E) (T 1 'x' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True

  , testCase "" $
      let act = insert 'x' (E :: HeapData Char)
          expe = (T 1 'x' E E)
      in compare_heap_data act expe @?= True
  , testCase "" $
      let act = insert 'x' (T 1 'y' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True
  , testCase "" $
      let act = insert 'y' (T 1 'x' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True

  , testCase "" $
      findMin (E :: HeapData Char) @?= Nothing
  , testCase "" $
      let h = insert 'a' $ insert 'x' (T 1 'y' E E)
      in findMin h @?= Just 'a'

  , testCase "" $
      isNothing (deleteMin (E :: HeapData Char)) @?= True
  , testCase "" $
      let h = insert 'a' $ insert 'x' (T 1 'y' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
          result = fromJust $ deleteMin h
      in compare_heap_data result expe @?= True
  ]

unitTestsForHelpers = testGroup "Unit tests"
  [ testCase "rank empty" $
      rank E @?= 0
  , testCase "rank not empty" $
      let h = foldr insert empty [2, 1..0]
      in rank h @?= 2

  , testCase "rank2 empty" $
      rank2 E @?= 0
  , testCase "rank2 not empty" $
      rank2 (T 1 'x' E E) @?= 1

  , testCase "checkRanks valid" $
      checkRanks (T 2 0 (T 1 1 E E) (T 1 2 E E)) @?= Right True
  , testCase "checkRanks invalid" $
      checkRanks (T 2 0 (E) (T 1 2 E E)) @?= Left "Rank 2 is invalid. 0 v.s. 1"

  , testCase "" $
      let act = insert2 'x' (E :: HeapData Char)
          expe = (T 1 'x' E E)
      in compare_heap_data act expe @?= True
  , testCase "" $
      let act = insert2 'x' (T 1 'y' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True
  , testCase "" $
      let act = insert2 'y' (T 1 'x' E E)
          expe = (T 1 'x' (T 1 'y' E E) E)
      in compare_heap_data act expe @?= True

  , testCase "fromList empty" $
      let h = (fromList [] :: HeapData Int)
      in isEmpty h @?= True
  , testCase "fromList not empty" $
      let h = fromList [2]
      in compare_heap_data h (T 1 2 E E) @?= True
  , testCase "fromList not empty" $
      let h = fromList [0..2]
      in compare_heap_data h (T 2 0 (T 1 1 E E) (T 1 2 E E)) @?= True
  ]
