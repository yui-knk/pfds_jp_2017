-- dataにtype constraintをつけるという話については以下を参照。
-- https://stackoverflow.com/questions/32828483/how-do-you-allow-gadts-in-haskell
-- https://wiki.haskell.org/Data_declaration_with_constraint
-- すごいHaskell p.123

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module BinaryTree
( UnbalancedSet
) where

class Ordered o where
    eq  :: o -> o -> Bool
    lt  :: o -> o -> Bool
    leq :: o -> o -> Bool

instance Ordered Int where
    eq  l r = l == r
    lt  l r = l <  r
    leq l r = l <= r

instance Ordered Char where
    eq  l r = l == r
    lt  l r = l <  r
    leq l r = l <= r

class Set s a where
    empty  :: s a
    insert :: a -> s a -> s a
    member :: a -> s a -> Bool

data UnbalancedSet e = E | T (UnbalancedSet e) e (UnbalancedSet e) deriving(Show)

instance Ordered a => Set (UnbalancedSet) a where
    empty = E

    -- > (insert 'c' $ insert 'b' $ insert 'a' empty) :: UnbalancedSet Char
    --   T E 'a' (T E 'b' (T E 'c' E))
    insert a E = T E a E
    insert a s@(T l e r)
        | a `lt` e = T (insert a l) e r
        | e `lt` a = T l e (insert a r)
        | otherwise = s

    -- > member 'd' (insert 'c' $ insert 'b' $ insert 'a' (empty :: UnbalancedSet Char))
    --   False
    member a E = False
    member a (T l e r)
        | a `lt` e = member a l
        | e `lt` a = member a r
        | otherwise = True

