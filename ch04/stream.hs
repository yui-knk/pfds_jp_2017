import Prelude hiding ((++), take, drop, reverse)

data StreamCell a = Nil | Cons a (StreamCell a) deriving(Show)

(++) :: StreamCell a -> StreamCell a -> StreamCell a
Nil ++ t = t
(Cons x s) ++ t = Cons x (s ++ t)

take :: Int -> StreamCell a -> StreamCell a
take 0 s = s
take _ Nil = Nil
take n (Cons x s) = Cons x (take (n - 1) s)

drop :: Int -> StreamCell a -> StreamCell a
drop 0 s = s
drop _ Nil = Nil
drop n (Cons x s) = drop (n - 1) s


drop2 :: Int -> StreamCell a -> StreamCell a
drop2 n s = drop' n s
    where drop' 0 s   = s
          drop' _ Nil = Nil
          drop' n (Cons x s) = drop' (n - 1) s

reverse :: StreamCell a -> StreamCell a
reverse s = reverse' s Nil
    where reverse' Nil r = r
          reverse' (Cons x s) r = reverse' s (Cons x r)

insertion_sort :: Ord a => StreamCell a -> StreamCell a
insertion_sort s = insertion_sort' s Nil
    where insert e Nil = Cons e Nil
          insert e c@(Cons x s) =
              if e > x then Cons x (insert e s)
              else Cons e c
          insertion_sort' Nil d = d
          insertion_sort' (Cons x s) d = insertion_sort' s $ insert x d
