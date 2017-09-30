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

--partition 2 (T (T (T E 1 E) 2 (T E 3 E)) 4 (T E 5 E))
partition :: Ord a => a -> SplayHeap a -> (SplayHeap a, SplayHeap a)
partition pivot E = (E, E)
partition pivot t@(T a x b) =
    if x <= pivot then
        case b of
            E -> (t, E)
            (T b1 y b2) ->
                if y <= pivot then
                    let (small, big) = partition pivot b2 in
                        (T (T a x b1) y small, big)
                else
                    let (small, big) = partition pivot b1 in
                        (T a x small, T big y b2)
    else
        case a of
            E -> (E, t)
            (T a1 y a2) ->
                if y <= pivot then
                    let (small, big) = partition pivot a2 in
                        (T a1 y small, T big x b)
                else
                    let (small, big) = partition pivot a1 in
                        (small, T big y (T a2 x b))

-- insert 4 (T (T (T E 1 E) 2 (T E 3 E)) 4 (T E 5 E))
-- insert 5 (T (T (T E 1 E) 4 (T E 6 E)) 10 (T E 13 E))
insert :: Ord a => a -> SplayHeap a -> SplayHeap a
insert x t = let (small, big) = partition x t in T small x big

-- merge (T (T E 4 E) 6 (T E 10 E)) (T E 1 (T E 13 E))
merge :: Ord a => SplayHeap a -> SplayHeap a -> SplayHeap a
merge E t = t
merge (T a x b) t =
    let (small, big) = partition x t in T (merge small a) x (merge big b)

-- findMin (T (T (T E 1 E) 2 (T E 3 E)) 4 (T E 5 E))
findMin :: SplayHeap a -> a
findMin (T E x b) = x
findMin (T a x b) = findMin a

-- deleteMin (T E 10 (T E 12 E))
-- deleteMin (T (T E 5 (T E 8 E)) 10 (T E 12 E))
-- deleteMin (T (T (T E 2 E) 5 (T E 8 E)) 10 (T E 12 E))
deleteMin :: SplayHeap a -> SplayHeap a
deleteMin (T E x b) = b
deleteMin (T (T E x b) y c) = T b y c
deleteMin (T (T a x b) y c) = T (deleteMin a) x (T b y c)
