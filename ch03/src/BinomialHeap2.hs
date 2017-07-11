module BinomialHeap2 where

data Tree e = Node e [Tree e] deriving(Show)
type Elem e = (Int, Tree e)

link :: Ord e => Elem e -> Elem e -> Elem e
link (r, t1@(Node x1 c1)) (_ ,t2@(Node x2 c2)) =
    if x1 <= x2
    then (r + 1, (Node x1 (t2:c1)))
    else (r + 1, (Node x2 (t1:c2)))

type Heap e = [Elem e]

rank :: Elem e -> Int
rank (i, _) = i

root :: Elem e -> e
root (_, (Node x c)) = x

empty :: Heap e
empty = []

insTree :: Ord e => Elem e -> Heap e -> Heap e
insTree t [] = [t]
insTree (i, t) ts@((r, t'):ts') =
    if i < r
    then (i, t):ts
    else insTree (link (i, t) (r, t')) ts'

insert :: Ord e => e -> Heap e -> Heap e
insert e ts = insTree (0, Node e []) ts

merge :: Ord e => Heap e -> Heap e -> Heap e
merge t [] = t
merge [] t = t
merge ts1@(t1':ts1') ts2@(t2':ts2')
    | rank t1' < rank t2' = t1':(merge ts1' ts2)
    | rank t1' > rank t2' = t2':(merge ts2' ts1)
    | otherwise = insTree (link t1' t2') (merge ts1' ts2')

removeMinTree :: Ord e => Heap e -> Maybe (Elem e, Heap e)
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = do -- tはrankが最も小さい
    (t', ts') <- removeMinTree ts
    if root t < root t' then return (t, ts) else return (t', t:ts')

findMin :: Ord e => Heap e -> Maybe e
findMin h = do
    (t, _) <- removeMinTree h
    return $ root t

treeToHeap :: Ord e => [Tree e] -> Heap e
treeToHeap ts = map (uncurry (,)) $ zip [0..] ts

deleteMin :: Ord e => Heap e -> Maybe (Heap e)
deleteMin h = do
    ((r, Node e c), ts) <- removeMinTree h
    return $ merge (treeToHeap $ reverse c) ts
