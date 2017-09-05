data Heap a = E | T a [Heap a] deriving(Show)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

-- merge (insert 11 $ insert 10 empty) (insert 1 $ insert 3 empty)
merge :: Ord a => Heap a -> Heap a -> Heap a
merge E t = t
merge t E = t
merge (h1@(T x hs1)) (h2@(T y hs2)) =
    if x <= y
    then T x (h2 : hs1)
    else T y (h1 : hs2)

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge (T x []) h

-- mergePairs [(T 1 []), (T 2 []), (T 3 []), (T 4 []), (T 5 []), (T 6 [])]
mergePairs :: Ord a => [Heap a] -> Heap a
mergePairs [] = E
mergePairs [h] = h
mergePairs (h1:h2:hs) = merge (merge h1 h2) (mergePairs hs)

findMin :: Heap a -> Maybe a
findMin E = Nothing
findMin (T x h) = Just x
