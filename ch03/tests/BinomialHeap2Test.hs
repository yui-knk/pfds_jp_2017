module BinomialHeap2Test
( unitTestsForInterfaces
) where


import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import BinomialHeap2

compare_tree_date :: Ord e => Tree e -> Tree e -> Bool
compare_tree_date (Node x1 c1) (Node x2 c2) =
   (x1 == x2) && (length c1 == length c2) && (all (\(a,b) -> compare_tree_date a b) $ zip c1 c2)

compare_heap_data :: Ord e => Heap e -> Heap e -> Bool
compare_heap_data [] [] = True
compare_heap_data [] _ = False
compare_heap_data _ [] = False
compare_heap_data h1 h2 =
    (length h1 == length h2) && (all (\((ia, a), (ib, b)) -> (ia == ib) && compare_tree_date a b) $ zip h1 h2)

unitTestsForInterfaces = testGroup "Unit tests"
  [ testCase "insert empty heap" $
      let act = insert 'b' []
          expe = [(0, (Node 'b' []))]
      in compare_heap_data act expe @?= True
  , testCase "insert not empty heap" $
      let act = insert 'b' $ insert 'a' empty
          expe = [(1, (Node 'a' [Node 'b' []]))]
      in compare_heap_data act expe @?= True

  , testCase "merge empty heap" $
      let act = merge [(0, (Node 'b' []))] []
          expe = [(0, (Node 'b' []))]
      in compare_heap_data act expe @?= True
  , testCase "merge empty heap" $
      let act = merge [] [(0, (Node 'b' []))]
          expe = [(0, (Node 'b' []))]
      in compare_heap_data act expe @?= True
  , testCase "merge not empty heap" $
      let act = merge [(1, (Node 'd' [(Node 'e' [])]))] [(0, (Node 'a' []))]
          expe = [(0, (Node 'a' [])), (1, (Node 'd' [(Node 'e' [])]))]
      in compare_heap_data act expe @?= True
  , testCase "merge not empty heap" $
      let act = merge [(1, (Node 'f' [(Node 'u' [])]))] [(1, (Node 'b' [(Node 'd' [])]))]
          expe =  [ (2, (Node 'b'
                      [ (Node 'f' [(Node 'u' [])])
                      , (Node 'd' [])
                      ]
                    ))
                  ]
      in compare_heap_data act expe @?= True

  , testCase "findMin empty heap" $
      isNothing (findMin ([] :: [Elem Char])) @?= True
  , testCase "findMin not empty heap" $
      findMin [(0, (Node 'b' []))] @?= Just 'b'
  , testCase "findMin not empty heap" $
      let h = [(0, (Node 'h' [])), (1, (Node 'd' [Node 'e' []]))]
      in findMin h @?= Just 'd'

  , testCase "deleteMin empty heap" $
      isNothing (deleteMin ([] :: [Elem Char])) @?= True
  , testCase "deleteMin not empty heap" $
      let result = fromJust (deleteMin [(1, (Node 'd' [Node 'e' []]))])
      in compare_heap_data result [(0, (Node 'e' []))] @?= True
  , testCase "deleteMin not empty heap" $
      let h = [(0, (Node 'h' [])), (1, (Node 'd' [Node 'g' []]))]
          result = fromJust (deleteMin h)
      in compare_heap_data result [(1, (Node 'g' [Node 'h' []]))] @?= True
  ]
