{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Heap where

class Heap h e where
    empty     :: h e
    isEmpty   :: h e -> Bool

    insert    :: e -> h e -> h e
    merge     :: h e -> h e -> h e

    findMin   :: h e -> Maybe e
    deleteMin :: h e -> Maybe (h e)
