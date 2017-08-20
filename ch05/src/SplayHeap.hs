data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving(Show)

empty :: SplayHeap a
empty = E

isEmpty :: SplayHeap a -> Bool
isEmpty E = True
isEmpty _ = False

--smaller :: a -> SplayHeap a -> SplayHeap a
--smaller x E = E
--smaller x t 

bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
bigger pivot E = E
bigger pivot (T a x b) =
    if x <= pivot
    then bigger pivot b
    else T (bigger x a) x b

--insert :: a -> SplayHeap a -> SplayHeap a
--insert x t = T (smaller x t) x (bigger x t)



