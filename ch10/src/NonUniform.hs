data Seq a = Nil | Cons a (Seq (a, a)) deriving(Show)

size :: Seq a -> Int
size Nil = 0
size (Cons x s) = 1 + 2 * (size s)

--size (Cons 0 (Cons (1, 2) Nil))
