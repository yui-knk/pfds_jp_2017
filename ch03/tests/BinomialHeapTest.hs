module BinomialHeapTest
( unitTestsForInterfaces
) where


import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import BinomialHeap

compare_tree_date :: Ord e => Tree e -> Tree e -> Bool
compare_tree_date (Node _ x1 c1) (Node _ x2 c2) =
    (x1 == x2) && (length c1 == length c2) && (all (\(a,b) -> compare_tree_date a b) $ zip c1 c2)

compare_heap_data :: Ord e => Heap e -> Heap e -> Bool
compare_heap_data [] [] = True
compare_heap_data [] _ = False
compare_heap_data _ [] = False
compare_heap_data h1 h2 =
    (length h1 == length h2) && (all (\(a,b) -> compare_tree_date a b) $ zip h1 h2)

unitTestsForInterfaces = testGroup "Unit tests"
  [ testCase "insTree empty heap" $
      let act = insTree (Node 0 'a' []) []
          expe = [(Node 0 'a' [])]
      in compare_heap_data act expe @?= True
  , testCase "insTree not empty heap" $
      let act = insTree (Node 0 'b' []) [Node 0 'a' []]
          expe = [(Node 0 'a' [Node 0 'b' []])]
      in compare_heap_data act expe @?= True

  , testCase "insert empty heap" $
      let act = insert 'b' []
          expe = [(Node 0 'b' [])]
      in compare_heap_data act expe @?= True
  , testCase "insTree not empty heap" $
      let act = insert 'b' [Node 0 'a' []]
          expe = [(Node 0 'a' [Node 0 'b' []])]
      in compare_heap_data act expe @?= True

  , testCase "merge empty heap" $
      let act = merge [(Node 0 'b' [])] []
          expe = [(Node 0 'b' [])]
      in compare_heap_data act expe @?= True
  , testCase "merge empty heap" $
      let act = merge [] [(Node 0 'b' [])]
          expe = [(Node 0 'b' [])]
      in compare_heap_data act expe @?= True
  , testCase "merge not empty heap" $
      let act = merge [(Node 1 'd' [Node 0 'e' []])] [(Node 0 'a' [])]
          expe = [(Node 0 'a' []), (Node 1 'd' [Node 0 'e' []])]
      in compare_heap_data act expe @?= True
  , testCase "merge not empty heap" $
      let act = merge [(Node 1 'f' [Node 0 'u' []])] [(Node 1 'b' [Node 0 'd' []])]
          expe = [(Node 2 'b' [(Node 1 'f' [Node 0 'u' []]), Node 0 'd' []])]
      in compare_heap_data act expe @?= True

  , testCase "findMin empty heap" $
      isNothing (findMin ([] :: [Tree Char])) @?= True
  , testCase "findMin not empty heap" $
      findMin [(Node 0 'b' [])] @?= Just 'b'
  , testCase "findMin not empty heap" $
      let h = [(Node 0 'h' []), (Node 1 'd' [Node 0 'e' []])]
      in findMin h @?= Just 'd'

  , testCase "deleteMin empty heap" $
      isNothing (deleteMin ([] :: [Tree Char])) @?= True
  , testCase "deleteMin not empty heap" $
      let result = fromJust (deleteMin [(Node 1 'd' [Node 0 'e' []])])
      in compare_heap_data result [(Node 0 'e' [])] @?= True
  , testCase "deleteMin not empty heap" $
      let h = [(Node 0 'h' []), (Node 1 'd' [Node 0 'g' []])]
          result = fromJust (deleteMin h)
      in compare_heap_data result [(Node 1 'g' [Node 0 'h' []])] @?= True
  ]
