{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module WeightBiasedLeftistHeap where

class Heap h e where
    empty     :: h e
    isEmpty   :: h e -> Bool

    insert    :: e -> h e -> h e
    merge     :: h e -> h e -> h e

    findMin   :: h e -> Maybe e
    deleteMin :: h e -> Maybe (h e)

data HeapData e = E | T Int e (HeapData e) (HeapData e) deriving(Show)

size :: HeapData e -> Int
size E = 0
size (T s _ _ _) = s

size2 :: HeapData e -> Int
size2 E = 0
size2 (T _ _ a b) = 1 + (size2 a) + (size2 b)

makeT :: Ord e => e -> HeapData e -> HeapData e -> HeapData e
makeT x a b = if size a >= size b then T size_sum x a b else T size_sum x b a
    where size_sum = size a + size b

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
