import Prelude hiding (head, tail)

data Queue a = Queue [a] [a] deriving(Show)

empty :: Queue a
empty = Queue [] []

isEmpty :: Queue a -> Bool
isEmpty (Queue f r) = null f

checkf :: Queue a -> Queue a
checkf (Queue [] r) = Queue (reverse r) []
checkf q = q

snoc :: Queue a -> a -> Queue a
snoc (Queue f r) x = checkf $ Queue f (x:r)

head :: Queue a -> Maybe a
head (Queue [] _) = Nothing
head (Queue (x:f) r) = Just x

tail :: Queue a -> Maybe (Queue a)
tail (Queue [] _) = Nothing
tail (Queue (x:f) r) = Just (checkf $ Queue f r)


