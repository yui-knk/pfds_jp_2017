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

