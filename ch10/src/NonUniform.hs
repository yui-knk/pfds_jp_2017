import Prelude hiding (head, tail, lookup)

data Seq a = Nil | Zero (Seq (a, a)) | One a (Seq (a, a)) deriving(Show)

size :: Seq a -> Int
size Nil = 0
size (Zero s) = 2 * size s
size (One x s) = 1 + 2 * (size s)

--size (Zero (One (0) Nil)) -- this cause error
--size (Zero (One (0, 1) Nil))
--size (Zero (One (0, 1) (One ((2, 3), (4, 5)) Nil)))

cons :: a -> Seq a -> Seq a
cons x Nil = One x Nil
cons x (Zero s) = One x s
cons x (One y s) = Zero (cons (x, y) s)

--cons 0 Nil
--cons 1 (cons 0 Nil)
--cons 2 (cons 1 (cons 0 Nil))
--cons 3 (cons 2 (cons 1 (cons 0 Nil)))

uncons :: Seq a -> (a, Seq a)
uncons (One x Nil) = (x, Nil)
uncons (One x s) = (x, Zero s)
uncons (Zero s) = (x, One y s')
    where ((x, y), s') = uncons s

head :: Seq a -> a
head s = x
    where (x, _) = uncons s

tail :: Seq a -> Seq a
tail s = s'
    where (_, s') = uncons s

lookup :: Int -> Seq a -> a
lookup 0 (One x s) = x
lookup i (One x s) = lookup (i-1) (Zero s)
lookup i (Zero s) = if (mod i 2) == 0 then x else y
    where (x, y) = lookup (div i 2) s

fupdate :: (a -> a) -> Int -> Seq a -> Seq a
fupdate f 0 (One x s) = One (f x) s
fupdate f i (One x s) = cons x (fupdate f (i-1) (Zero s))
fupdate f i (Zero s) = Zero (fupdate f' (div i 2) s)
    where
      f' = \(x, y) -> if (mod i 2) == 0 then (f x, y) else (x, f y)

update :: Int -> a -> Seq a -> Seq a
update i y s = fupdate (\x -> y) i s

-- update 0 10 (cons 3 (cons 2 (cons 1 (cons 0 Nil))))
-- update 1 10 (cons 3 (cons 2 (cons 1 (cons 0 Nil))))
-- update 2 10 (cons 3 (cons 2 (cons 1 (cons 0 Nil))))
-- update 3 10 (cons 3 (cons 2 (cons 1 (cons 0 Nil))))
