import Prelude hiding (head, tail)

data Queue a = E | Q Int [a] (Queue [a]) Int [a] deriving(Show)

empty :: Queue a
empty = E

isEmpty :: Queue a -> Bool
isEmpty E = True
isEmpty _ = False

checkQ :: Int -> [a] -> Queue [a] -> Int -> [a] -> Queue a
checkQ lenfm f m lenr r =
    if lenr <= lenfm
    then checkF lenfm f m lenr r
    else checkF (lenfm+lenr) f (snoc m (reverse r)) 0 []

checkF :: Int -> [a] -> Queue [a] -> Int -> [a] -> Queue a
checkF lenfm [] E lenr r = E
checkF lenfm [] m lenr r = Q lenfm (head m) (tail m) lenr r
checkF lenfm f m lenr r = Q lenfm f m lenr r

snoc :: Queue a -> a -> Queue a
snoc E x = Q 1 [x] E 0 []
snoc (Q lenfm f m lenr r) x = checkQ lenfm f m (lenr+1) (x:r)

head :: Queue a -> a
head (Q lenfm (x:f') m lenr r) = x

tail :: Queue a -> Queue a
tail (Q lenfm (x:f') m lenr r) = checkQ (lenfm-1) f' m lenr r

--foldl (snoc) empty [1..10]

-- 10_3
--foldl (snoc) empty [1..16]

