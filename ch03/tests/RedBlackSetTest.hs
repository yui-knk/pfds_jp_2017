module RedBlackSetTest
( unitTestsForInterfaces
) where


import Test.Tasty
import Test.Tasty.HUnit
import Data.Maybe

import RedBlackSet

compare_tree_data :: Ord e => Tree e -> Tree e -> Bool
compare_tree_data E E = True
compare_tree_data E _ = False
compare_tree_data _ E = False
compare_tree_data (T c1 a1 x1 b1) (T c2 a2 x2 b2) =
    (c1 == c2) && (x1 == x2) && (compare_tree_data a1 a2) && (compare_tree_data b1 b2)


unitTestsForInterfaces =
    let a = T Black E 2 E
        b = T Black E 7 E
        c = T Black E 12 E
        d = T Black E 17 E
        expe = T Red (T Black a 5 b) 10 (T Black c 15 d)

    in testGroup "Unit tests"
        [ testCase "balance" $
              let act = balance Black (T Red (T Red a 5 b) 10 c) 15 d 
              in compare_tree_data act expe @?= True
        , testCase "balance" $
              let act = balance Black (T Red a 5 (T Red b 10 c)) 15 d 
              in compare_tree_data act expe @?= True
        , testCase "balance" $
              let act = balance Black a 5 (T Red (T Red b 10 c) 15 d)
              in compare_tree_data act expe @?= True
        , testCase "balance" $
              let act = balance Black a 5 (T Red b 10 (T Red c 15 d))
              in compare_tree_data act expe @?= True
        ]
