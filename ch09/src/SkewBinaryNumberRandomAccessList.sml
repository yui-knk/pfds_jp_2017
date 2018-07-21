use "RANDOMACCESSLIST.sml";

structure SkewBinaryNumberRandomAccessList : RANDOMACCESSLIST =
struct
  datatype 'a Tree = LEAF of 'a | NODE of 'a * 'a Tree * 'a Tree
  type 'a RList = (int * 'a Tree) list

  val empty = []
  fun isEmpty ts = null ts

  fun cons (x, ts as (w1, t1) :: (w2, t2) :: ts') =
        if w1 = w2 then (1 + w1 + w2, NODE (x, t1, t2)) :: ts'
        else (1, LEAF x) :: ts
    | cons (x, ts) = (1, LEAF x) :: ts

  fun head [] = raise Empty
    | head ((1, LEAF x) :: ts) = x
    | head ((w, NODE (x, t1, t2)) :: ts) = x

  fun tail [] = raise Empty
    | tail ((1, LEAF x) :: ts) = ts
    | tail ((w, NODE (x, t1, t2)) :: ts) = (w div 2, t1) :: (w div 2, t2) :: ts

  fun lookupTree (1, 0, LEAF x) = x
    | lookupTree (1, i, LEAF x) = raise Subscript
    | lookupTree (w, 0, NODE (x, t1, t2)) = x
    | lookupTree (w, i, NODE (x, t1, t2)) =
        if i <= w div 2 then lookupTree (w div 2, i - 1, t1)
        else lookupTree (w div 2, i - 1 - w div 2, t2)
          (* 2引数目の引き算は、いくつのelmeをみたことにするかということ *)

  fun updateTree (1, 0, y, LEAF x) = LEAF y
    | updateTree (1, i, y, LEAF x) = raise Subscript
    | updateTree (w, 0, y, NODE (x, t1, t2)) = NODE (y, t1, t2)
    | updateTree (w, i, y, NODE (x, t1, t2)) =
        if i <= w div 2 then NODE (x, updateTree (w div 2, i - 1, y, t1), t2)
        else NODE (x, t1, updateTree (w div 2, i - 1 - w div 2, y, t2))

  fun lookup (i, []) = raise Subscript
    | lookup (i, (w, t) :: ts) =
        if i < w then lookupTree (w, i, t)
        else lookup (i - w, ts)

  fun update (i, y, []) = raise Subscript
    | update (i, y, (w, t) :: ts) =
        if i < w then (w, updateTree (w, i, y, t)) :: ts
        else (w, t) :: update (i - w, y, ts)
end
