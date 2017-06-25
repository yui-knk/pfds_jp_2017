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
    insert :: a -> s a -> Maybe (s a)
    member :: a -> s a -> Bool

data UnbalancedSet e = E | T (UnbalancedSet e) e (UnbalancedSet e) deriving(Show)

insert_internal :: Ordered a => a -> UnbalancedSet a -> UnbalancedSet a
insert_internal a E = T E a E
insert_internal a s@(T l e r)
    | a `lt` e = T (insert_internal a l) e r
    | e `lt` a = T l e (insert_internal a r)
    | otherwise = s

member_internal :: Ordered a => a -> UnbalancedSet a -> a -> Bool
member_internal a E can = a `eq` can
member_internal a (T l e r) can
    | a `leq` e = member_internal a l e
    | otherwise = member_internal a r can

instance Ordered a => Set (UnbalancedSet) a where
    empty = E

    -- > (insert 'a' empty >>= (insert 'a') >>= (insert 'c')) :: Maybe (UnbalancedSet Char)
    --   Nothing
    -- > (insert 'a' empty >>= (insert 'b') >>= (insert 'c')) :: Maybe (UnbalancedSet Char)
    --   Just (T E 'a' (T E 'b' (T E 'c' E)))
    insert a s
        | member a s = Nothing
        | otherwise = Just $ insert_internal a s

    -- > member 'd' (insert 'c' $ insert 'b' $ insert 'a' (empty :: UnbalancedSet Char))
    --   False
    member a E = False
    member a t@(T l e r) = member_internal a t e


