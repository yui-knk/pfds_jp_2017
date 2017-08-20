data SplayHeap a = E | T (SplayHeap a) a (SplayHeap a) deriving(Show)

empty :: SplayHeap a
empty = E

isEmpty :: SplayHeap a -> Bool
isEmpty E = True
isEmpty _ = False

-- smaller includes elements which are euqal to pivot
--
-- smaller 5 (T (T (T E 1 E) 3 E) 4 (T (T E 5 E) 8 (T E 10 E)))
-- smaller 8 (T E 1 (T E 2 (T E 3 (T E 4 (T E 5 (T E 6 (T E 7 E)))))))
smaller :: Ord a => a -> SplayHeap a -> SplayHeap a
smaller pivot E = E
smaller pivot (T a x b) =
    if x > pivot
    then smaller pivot a
    else case b of
        E -> T a x E
        (T a1 y a2) ->
            if y > pivot
            then T a x (smaller pivot a1)
            else T (T a x a1) y (smaller pivot a2)


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



