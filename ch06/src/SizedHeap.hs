-- ref: https://github.com/rst76/pfds/blob/master/ch03/ExplicitMinHeap.hs

module SizedHeap(SizedHeap) where

import qualified Heap as H

data SizedHeap h i a = E | NE i (h a) deriving Show

instance (H.Heap h, Num i) => H.Heap (SizedHeap h i) where

    empty = E

    isEmpty E = True
    isEmpty _ = False

    insert x E        = NE 1 (H.insert x H.empty)
    insert x (NE s h) = NE (s + 1) (H.insert x h)

    merge h E = h
    merge E h = h
    merge (NE s1 h1) (NE s2 h2) = NE (s1 + s2) (H.merge h1 h2)

    findMin E        = error "empty heap"
    findMin (NE _ h) = H.findMin h

    deleteMin E = error "empty heap"
    deleteMin (NE s h)
        | H.isEmpty h' = E
        | otherwise    = NE (s - 1) h'
        where
        h' = H.deleteMin h
