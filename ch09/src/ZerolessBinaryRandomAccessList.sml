use "RANDOMACCESSLIST.sml";

(*structure ZerolessBinaryRandomAccessList : RANDOMACCESSLIST =*)
structure ZerolessBinaryRandomAccessList = 
struct
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  datatype 'a Digit = ONE of 'a Tree | TWO of 'a Tree * 'a Tree
  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)

  fun consTree (t, []) = [ONE t]
    | consTree (t1, ONE t2 :: ts) = TWO (t1, t2) :: ts
    | consTree (t1, TWO (t2, t3) :: ts) = ONE t1 :: consTree (link (t2, t3), ts)

  fun unconsTree [] = raise Empty
    | unconsTree [ONE t] = (t, [])
    | unconsTree (TWO (t1, t2) :: ts) = (t1, ONE t2 :: ts)
    | unconsTree (ONE t :: ts) =
        let val (NODE (_, t1, t2), ts') = unconsTree ts
        in (t, TWO (t1, t2) :: ts') end

  fun cons (x, ts) = consTree (LEAF x, ts)
  fun head [] = raise Empty
    | head (ONE (LEAF x) :: ts) = x
    | head (TWO (LEAF x, LEAF y) :: ts) = x
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF x) = raise Subscript
    | lookupTree (i, NODE (w, t1, t2)) =
        if i < w div 2 then lookupTree (i, t1)
        else lookupTree (i - w div 2, t2)

  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (i, y, LEAF x) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) =
        if i < w div 2 then NODE (w, updateTree (i, y, t1), t2)
        else NODE (w, t1, updateTree (i - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, ONE t :: ts) =
        if i < size t then lookupTree (i, t) else lookup (i - size t, ts)
    | lookup (i, TWO (t1, t2) :: ts) =
        if i < size t1 then lookupTree (i, t1)
        else if (i - size t1) < size t2 then lookupTree (i - size t1, t2)
        else lookup (i - size t1 - size t2, ts)

  fun update (i, y, []) = raise Subscript
    | update (i, y, ONE t :: ts) =
        if i < size t then ONE (updateTree (i, y, t)) :: ts
        else ONE t :: update (i - size t, y, ts)
    | update (i, y, TWO (t1, t2) :: ts) =
        if i < size t1 then TWO (updateTree (i, y, t1), t2) :: ts
        else if (i - size t1) < size t2 then TWO (t1, updateTree (i - size t1, y, t2)) :: ts
        else TWO (t1, t2) :: update (i - size t1 - size t2, y, ts)
end

val t = foldl ZerolessBinaryRandomAccessList.cons ZerolessBinaryRandomAccessList.empty [1,2,3,4]
val e0 = ZerolessBinaryRandomAccessList.lookup (0, t)
val e1 = ZerolessBinaryRandomAccessList.lookup (1, t)
val e2 = ZerolessBinaryRandomAccessList.lookup (2, t)
val e3 = ZerolessBinaryRandomAccessList.lookup (3, t)
