module Tree
( complete
) where

data Tree a = E | T (Tree a) a (Tree a) deriving(Show)


-- (a)

complete :: e -> Int -> Tree e
complete x d
    | d <= 0 = E
    | otherwise = T t x t
    where t = complete x (d - 1)

-- (b)
create2 :: e -> Int -> (Tree e, Tree e)
create2 x m = (complete2 x m, complete2 x (m + 1))

-- O(log n)??
complete2 :: e -> Int -> Tree e
complete2 x s
    | s <= 0 = E
    | even s = T b x a
    | otherwise = T a x a
    where (a, b) = create2 x ((s - 1) `quot` 2)


-- debug
depth :: Tree e -> Int
depth E = 0
depth (T a x b) = max ((depth a) + 1) ((depth b) + 1)

depths :: Tree e -> Tree Int
depths E = E
depths (T a x b) = T (depths a) m (depths b)
    where da = depth a
          db = depth b
          m = max da db
