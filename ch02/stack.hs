module Stack
( ListStack
, CustomStack
) where

class Stack s where
    empty   :: s a
    isEmpty :: s a -> Bool
    cons    :: a -> s a -> s a
    head    :: s a -> Maybe a
    tail    :: s a -> Maybe (s a)

data ListStack a = ListStack [a] deriving(Show)

instance Stack ListStack where
    empty = ListStack []
    isEmpty (ListStack a) = null a
    cons a (ListStack s) = ListStack (a:s)

    head (ListStack []) = Nothing
    head (ListStack s) = Just $ Prelude.head s

    tail (ListStack []) = Nothing
    tail (ListStack s) = Just $ ListStack (Prelude.tail s)

data CustomStack a = Nil | Cons a (CustomStack a) deriving(Show)

instance Stack CustomStack where
    empty = Nil

    isEmpty Nil = True
    isEmpty _ = False

    cons a s = Cons a s

    head Nil = Nothing
    head (Cons a b) = Just a

    tail Nil = Nothing
    tail (Cons a b) = Just b
