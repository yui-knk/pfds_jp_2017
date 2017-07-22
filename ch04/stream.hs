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
