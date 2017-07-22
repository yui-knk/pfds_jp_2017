{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module RedBlackSet
where

data Color = Red | Black deriving(Show, Eq)
data Tree e = E | T Color (Tree e) e (Tree e) deriving(Show)

empty :: Tree e
empty = E

member :: Ord e => e -> Tree e -> Bool
member _ E = False
member x (T _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

balance :: Color -> Tree e -> e -> Tree e -> Tree e
balance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d) -- left
balance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d) -- top
balance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d) -- bottom
balance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d) -- right
balance color a x b = T color a x b

insert :: Ord e => e -> Tree e -> Tree e
insert x s = T Black a y b
    where ins E = T Red E x E
          ins s@(T color a y b)
              | x < y = balance color (ins a) y b
              | x > y = balance color a y (ins b)
              | otherwise = s
          T _ a y b = ins s

fromOrdList :: Ord e => [e] -> Tree e
fromOrdList elms = foldl (flip insert) E elms
