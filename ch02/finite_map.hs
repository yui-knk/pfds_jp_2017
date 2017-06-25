{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module FiniteMap
( FiniteMap
) where

class FiniteMap m k where
    empty  :: m k a
    bind   :: k -> a -> m k a -> m k a
    lookup :: k -> m k a -> Maybe a

data Tree k a = E | Tree (k, a) (Tree k a) (Tree k a) deriving(Show)

instance Ord k => FiniteMap (Tree) k where
    empty = E

    bind k a E = Tree (k, a) E E
    bind k a (Tree (k2, v) l r)
        | k < k2 = Tree (k2, v) (bind k a l) r
        | k > k2 = Tree (k2, v) l (bind k a r)
        | otherwise = Tree (k, a) l r

    -- > FiniteMap.lookup 2 $ bind 1 'a' (empty :: Tree Int Char)
    --   Nothing
    -- > FiniteMap.lookup 1 $ bind 1 'a' (empty :: Tree Int Char)
    --   Just 'a'
    lookup k E = Nothing
    lookup k (Tree (k2, v) l r)
        | k < k2 = FiniteMap.lookup k l
        | k > k2 = FiniteMap.lookup k r
        | otherwise = Just v
