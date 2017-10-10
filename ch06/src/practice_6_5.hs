import Heap
import SizedHeap
import BinomialHeap

main :: IO ()
main = do
    print (foldr insert empty [1, 2, 3, 4, 5, 6, 7] :: SizedHeap BinomialHeap Int Int)
    -- => NE 7 (BH [Node 0 1 [],Node 1 2 [Node 0 3 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]])
    print (deleteMin $ foldr insert empty [1, 2, 3, 4, 5, 6, 7] :: SizedHeap BinomialHeap Int Int)
    -- => NE 6 (BH [Node 1 2 [Node 0 3 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]])
    print (merge (foldr insert empty [1, 2]) (foldr insert empty [3, 4, 5, 6, 7]) :: SizedHeap BinomialHeap Int Int)
    -- => NE 7 (BH [Node 0 3 [],Node 1 1 [Node 0 2 []],Node 2 4 [Node 1 6 [Node 0 7 []],Node 0 5 []]])
