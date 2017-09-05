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

-- deleteMin (T 1 [T 3 [T 5 [T 6 []],T 4 []],T 2 []])
deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin E = Nothing
deleteMin (T x hs) = Just $ mergePairs hs


data BinTree a = E' | T' a (BinTree a) (BinTree a) deriving(Show)

toBinaryI4 :: Heap a -> [Heap a] -> BinTree a
toBinaryI4 (T x [])             [] = T' x E'                  E'
toBinaryI4 (T x (h1:hs1))       [] = T' x (toBinaryI4 h1 hs1) E'
toBinaryI4 (T x [])       (h2:hs2) = T' x E'                  (toBinaryI4 h2 hs2)
toBinaryI4 (T x (h1:hs1)) (h2:hs2) = T' x (toBinaryI4 h1 hs1) (toBinaryI4 h2 hs2)

-- toBinary (T 1 [T 3 [T 5 [T 6 []],T 4 []],T 2 []])
--   T' 1 (T' 3 (T' 5 (T' 6 E' E') (T' 4 E' E')) (T' 2 E' E')) E'
toBinary :: Heap a -> BinTree a
toBinary E = E'
toBinary h = toBinaryI4 h []
