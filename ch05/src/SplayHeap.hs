data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving(Show)

empty :: SplayHeap a
empty = E

isEmpty :: SplayHeap a -> Bool
isEmpty E = True
isEmpty _ = False

--smaller :: a -> SplayHeap a -> SplayHeap a
--smaller x E = E
--smaller x t 


-- bigger 0 (T (T (T (T (T (T (T E 1 E) 2 E) 3 E) 4 E) 5 E) 6 E) 7 E)
bigger :: Ord a => a -> SplayHeap a -> SplayHeap a
bigger pivot E = E
bigger pivot (T a x b) =
    if x <= pivot
    then bigger pivot b
    else case a of
        E -> T E x b
        (T a1 y a2) ->
            if y <= pivot
            then T (bigger pivot a2) x b
            else T (bigger pivot a1) y (T a2 x b)

--insert :: a -> SplayHeap a -> SplayHeap a
--insert x t = T (smaller x t) x (bigger x t)



