import Prelude hiding (head, tail, last, init)

data Deque a = Deque [a] [a] deriving(Show)

empty :: Deque a
empty = Deque [] []

isEmpty :: Deque a -> Bool
isEmpty (Deque [] []) = True
isEmpty (Deque _ _ ) = False

-- balance [1,2,3] == ([1], [2, 3])
balance :: [a] -> ([a], [a])
balance [] = ([], [])
balance l = splitAt f_len l
    where f_len = div (length l) 2


checkf :: Deque a -> Deque a
checkf (Deque [] r) = let (a, b) = balance r in Deque (reverse b) a 
checkf (Deque f []) = let (a, b) = balance f in Deque a (reverse b)
checkf q = q

cons :: a -> Deque a -> Deque a
cons x (Deque f r) = checkf $ Deque (x:f) r

-- valid or invalid
--
-- []    []    : valid
-- []    [1]   : valid
-- [1]   []    : valid
-- [1,2] []    : invalid
-- []    [2,1] : invalid
-- [1]   [2]   : valid
--
-- so if `(Deque [] (x:r))`, this is `(Deque [] [e])`
-- r has only one element.

head :: Deque a -> Maybe a
head (Deque [] []) = Nothing
head (Deque [] (x:r)) = Just x
head (Deque (x:f) r)  = Just x

tail :: Deque a -> Maybe (Deque a)
tail (Deque [] []) = Nothing
tail (Deque [] (x:r)) = Just $ empty
tail (Deque (x:f) r)  = Just (checkf $ Deque f r)


snoc :: Deque a -> a -> Deque a
snoc (Deque f r) x = checkf $ Deque f (x:r)

---- [1, 2, 3, 4, 5, 6]
---- [1, 2, 3, 4, 5]    init
----                 6  last
last :: Deque a -> Maybe a
last (Deque [] []) = Nothing
last (Deque (x:f) []) = Just x
last (Deque f (x:r))  = Just x

init :: Deque a -> Maybe (Deque a)
init (Deque [] []) = Nothing
init (Deque (x:f) []) = Just $ empty
init (Deque f (x:r))  = Just (checkf $ Deque f r)

