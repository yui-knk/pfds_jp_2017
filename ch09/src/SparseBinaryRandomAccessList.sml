use "RANDOMACCESSLIST.sml";

structure SparseBinaryRandomAccessList : RANDOMACCESSLIST =
struct
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  type 'a RList = 'a Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)

  fun consTree (t, []) = [t]
    | consTree (t, ts as t' :: ts') =
        if size t < size t' then t :: ts
        else consTree (link (t, t'), ts')

  fun unconsTree (l as LEAF x, ts) = (l, ts)
    | unconsTree (NODE (_, t1, t2), ts) = unconsTree (t1, t2 :: ts)

  fun cons (x, ts) = consTree (LEAF x, ts)
  fun head [] = raise Empty
    | head (t :: ts) = let val (LEAF x, _) = unconsTree (t, ts) in x end
  fun tail [] = raise Empty
    | tail (t :: ts) = let val (_, ts') = unconsTree (t, ts) in ts' end

  (*
    この関数は1つのTreeについてindex i(0-based)で探索を行う。
    停止条件は、第二引数がLEAFであること。
  *)
  fun lookupTree (0, LEAF x) = x
    | lookupTree (i, LEAF x) = raise Subscript
    | lookupTree (i, NODE (w, t1, t2)) =
        if i < w div 2 then lookupTree (i, t1)
        else lookupTree (i - w div 2, t2)

  (*
    この関数は1つのTreeについてindex i(0-based)で更新を行う。
  *)
  fun updateTree (0, y, LEAF x) = LEAF y
    | updateTree (i, y, LEAF x) = raise Subscript
    | updateTree (i, y, NODE (w, t1, t2)) =
        if i < w div 2 then NODE (w, updateTree (i, y, t1), t2)
        else NODE (w, t1, updateTree (i - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, t :: ts) =
        if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

  fun update (i, y, []) = raise Subscript
    | update (i, y, t :: ts) =
        if i < size t then updateTree (i, y, t) :: ts
        else t :: update (i - size t, y, ts)
end

val t = foldl SparseBinaryRandomAccessList.cons SparseBinaryRandomAccessList.empty [1,2,3,4]
val e1 = SparseBinaryRandomAccessList.lookup (1, t)
