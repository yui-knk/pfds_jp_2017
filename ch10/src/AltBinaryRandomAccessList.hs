-- Ref: ch09/src/ZerolessRedundantBinaryRandomAccessList.sml

import Prelude hiding (head, tail, lookup)

--data Seq a = Nil | Zero (Seq (a, a)) | One a (Seq (a, a)) deriving(Show)
data RList a = NIL
             | ONE a (RList (a, a))
             | TWO a a (RList (a, a))
             | THREE a a a (RList (a, a)) 
             deriving(Show)

cons :: a -> RList a -> RList a
cons x NIL = ONE x NIL
cons x (ONE a s)       = TWO x a s
cons x (TWO a b s)     = THREE x a b s
cons x (THREE a b c s) = TWO x a (cons (b, c) s)

-- foldl (flip cons) NIL [1..10]

uncons :: RList a -> (a, RList a)
uncons (ONE a NIL)     = (a, NIL)
uncons (ONE a s)       = (a, TWO a' b' s')
    where ((a', b'), s') = uncons s
uncons (TWO a b s)     = (a, ONE b s)
uncons (THREE a b c s) = (a, TWO b c s)

head :: RList a -> a
head s = x
    where (x, _) = uncons s

tail :: RList a -> RList a
tail s = s'
    where (_, s') = uncons s

--lookup :: Int -> RList a -> a
--lookup 0 (ONE a s) = a
--lookup i (ONE a s) = lookup () s
--lookup 0 (TWO a b s) = a
--lookup 1 (TWO a b s) = b
--lookup i (TWO a b s) = lookup () s
--lookup 0 (THREE a b c s) = a
--lookup 1 (THREE a b c s) = b
--lookup 2 (THREE a b c s) = c
--lookup i (THREE a b c s) = lookup () s


--lookup :: Int -> Seq a -> a
--lookup 0 (One x s) = x
--lookup i (One x s) = lookup (i-1) (Zero s)
--lookup i (Zero s) = if (mod i 2) == 0 then x else y
--    where (x, y) = lookup (div i 2) s


--fupdate :: (a -> a) -> Int -> Seq a -> Seq a
--fupdate f 0 (One x s) = One (f x) s
--fupdate f i (One x s) = cons x (fupdate f (i-1) (Zero s))
--fupdate f i (Zero s) = Zero (fupdate f' (div i 2) s)
--    where
--      f' = \(x, y) -> if (mod i 2) == 0 then (f x, y) else (x, f y)

--update :: Int -> a -> Seq a -> Seq a
--update i y s = fupdate (\x -> y) i s
