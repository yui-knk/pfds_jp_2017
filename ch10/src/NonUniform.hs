data Seq a = Nil | Zero (Seq (a, a)) | One a (Seq (a, a)) deriving(Show)

size :: Seq a -> Int
size Nil = 0
size (Zero s) = 2 * size s
size (One x s) = 1 + 2 * (size s)

--size (Zero (One (0) Nil)) -- this cause error
--size (Zero (One (0, 1) Nil))
--size (Zero (One (0, 1) (One ((2, 3), (4, 5)) Nil)))
