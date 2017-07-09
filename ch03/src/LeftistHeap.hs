{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module LeftistHeap where

class Heap h e where
    empty     :: h e
    isEmpty   :: h e -> Bool

    insert    :: e -> h e -> h e
    merge     :: h e -> h e -> h e

    findMin   :: h e -> Maybe e
    deleteMin :: h e -> Maybe (h e)

data HeapData e = E | T Int e (HeapData e) (HeapData e) deriving(Show)

rank :: HeapData e -> Int
rank E = 0
rank (T i _ _ _) = i

rank2 :: HeapData e -> Int
rank2 E = 0
rank2 (T _ _ _ b) = 1 + rank2 b

makeT :: Ord e => e -> HeapData e -> HeapData e -> HeapData e
makeT x a b = if rank a >= rank b then T (1 + rank b) x a b else T (1 + rank a) x b a

invalidRankMessage :: Int -> Int -> Int -> String
invalidRankMessage rankSelf rankA rankB =
    "Rank " ++ (show rankSelf) ++ " is invalid. " ++ (show rankA) ++ " v.s. " ++ (show rankB)


checkRanks :: HeapData e -> Either String Bool
checkRanks E = Right True
checkRanks (T r x a b) = do
    _ <- (if rank2 a >= rank2 b then Right True else Left $ invalidRankMessage r (rank2 a) (rank2 b))
    _ <- (checkRanks a)
    checkRanks b


insert2 :: Ord e => e -> HeapData e -> HeapData e
insert2 x E = (T 1 x E E)
insert2 x (T _ y a b) =
    if x <= y
    then makeT x a $ insert2 y b
    else makeT y a $ insert2 x b

instance Ord e => Heap HeapData e where
    empty = E

    isEmpty E = True
    isEmpty _ = False

    merge E h = h
    merge h E = h
    merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
        if x <= y
        then makeT x a1 $ merge b1 h2
        else makeT y a2 $ merge h1 b2

    insert x h = merge (T 1 x E E) h

    findMin E = Nothing
    findMin (T _ x _ _) = Just x

    deleteMin E = Nothing
    deleteMin (T _ x a b) = Just $ merge a b
