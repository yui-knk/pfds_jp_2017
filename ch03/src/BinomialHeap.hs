module BinomialHeap where

data Tree e = Node Int e [Tree e] deriving(Show)

link :: Ord e => Tree e -> Tree e -> Tree e
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2
    then Node (r + 1) x1 (t2:c1)
    else Node (r + 1) x2 (t1:c2)

type Heap e = [Tree e]

rank :: Tree e -> Int
rank (Node r x c) = r

root :: Tree e -> e
root (Node r x c) = x

insTree :: Ord e => Tree e -> Heap e -> Heap e
insTree t [] = [t]
insTree t ts@(t':ts') =
    if rank t < rank t'
    then t:ts
    else insTree (link t t') ts'

insert :: Ord e => e -> Heap e -> Heap e
insert e ts = insTree (Node 0 e []) ts

merge :: Ord e => Heap e -> Heap e -> Heap e
merge t1 [] = t1
merge [] t1 = t1
merge ts1@(t1':ts1') ts2@(t2':ts2')
    | rank t1' < rank t2' = t1':(merge ts1' ts2)
    | rank t1' > rank t2' = t2':(merge ts2' ts1)
    | otherwise = insTree (link t1' t2') (merge ts1' ts2')

removeMinTree :: Ord e => Heap e -> Maybe (Tree e, Heap e)
removeMinTree [] = Nothing
removeMinTree [t] = Just (t, [])
removeMinTree (t:ts) = do -- tはrankが最も小さい
    (t', ts') <- removeMinTree ts
    if root t < root t' then return (t, ts) else return (t', t:ts')

findMin :: Ord e => Heap e -> Maybe e
findMin h = do
    (t, _) <- removeMinTree h
    return $ root t

findMin2 :: Ord e => Heap e -> Maybe e
findMin2 [] = Nothing
findMin2 h = Just $ minimum $ map root h

deleteMin :: Ord e => Heap e -> Maybe (Heap e)
deleteMin h = do
    ((Node r x c), ts) <- removeMinTree h
    return $ merge (reverse c) ts
