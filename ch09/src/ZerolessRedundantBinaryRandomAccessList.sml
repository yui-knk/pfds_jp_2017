use "RANDOMACCESSLIST.sml";

Control.lazysml := true;
open Lazy

fun force ($ x) = x

signature STREAM =
sig
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  val concat  : 'a Stream * 'a Stream -> 'a Stream
  val take    : int * 'a Stream -> 'a Stream
  val drop    : int * 'a Stream -> 'a Stream
  val reverse : 'a Stream -> 'a Stream
end

structure Stream : STREAM = 
struct
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  fun lazy concat (($NIL), t) = t
         | concat (($(CONS (x, s))), t) = $(CONS (x, concat (s, t)))

  fun lazy take (0, s) = $NIL
         | take (n, $NIL) = $NIL
         | take (n, $(CONS (x, s))) = $(CONS (x, take (n - 1, s)))

  fun lazy drop (n, s) =
      let fun drop' (0, s) = s
            | drop' (n, $NIL) = $NIL
            | drop' (n, $(CONS(x, s))) = drop' (n - 1, s)
      in drop'(n, s) end

  fun lazy reverse r =
      let fun reverse' ($NIL, r) = r
            | reverse' ($(CONS(x, s)), r) = reverse' (s, $(CONS(x, r)))
      in reverse' (r, $NIL) end
end

structure ZerolessRedundantBinaryRandomAccessList : RANDOMACCESSLIST =
struct 
  open Stream

  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  datatype 'a Digit =
         ONE of 'a Tree
       | TWO of 'a Tree * 'a Tree
       | THREE of 'a Tree * 'a Tree * 'a Tree

  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)

  fun consTree (t, []) = [ONE t]
    | consTree (t1, ONE (t2) :: ts)           = TWO (t1, t2) :: ts
    | consTree (t1, TWO (t2, t3) :: ts)       = THREE (t1, t2, t3) :: ts
    | consTree (t1, THREE (t2, t3, t4) :: ts) = TWO (t1, t2) :: consTree (link (t3, t4), ts)

  fun unconsTree ([]) = raise Empty
    | unconsTree ([ONE t]) = (t, [])
    | unconsTree (ONE (t1) :: ts)           =
        let val (NODE (_, t2, t3), ts') = unconsTree ts
        in (t1, TWO (t2, t3) :: ts') end
    | unconsTree (TWO (t1, t2) :: ts)       = (t1, ONE (t2) :: ts)
    | unconsTree (THREE (t1, t2, t3) :: ts) = (t1, TWO (t2, t3) :: ts)

  fun cons (x, ts) = consTree (LEAF x, ts)
  fun head ts = let val (LEAF x, _) = unconsTree ts in x end
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
    | lookup (i, ONE (t1) :: ts) =
        if i < size t1 then lookupTree (i, t1) else lookup (i - size t1, ts)
    | lookup (i, TWO (t1, t2) :: ts) =
        if i < size t1 then lookupTree (i, t1)
        else if (i - size t1) < size t2 then lookupTree (i - size t1, t2)
        else lookup (i - size t1 - size t2, ts)
    | lookup (i, THREE (t1, t2, t3) :: ts) =
        if i < size t1 then lookupTree (i, t1)
        else if (i - size t1) < size t2 then lookupTree (i - size t1, t2)
        else if (i - size t1 - size t2) < size t3 then lookupTree (i - size t1 - size t2, t3)
        else lookup (i - size t1 - size t2 - size t3, ts)

  fun update (i, y, []) = raise Subscript
    | update (i, y, ONE (t1) :: ts) =
        if i < size t1 then ONE (updateTree (i, y, t1)) :: ts
        else ONE (t1) :: update (i - size t1, y, ts)
    | update (i, y, TWO (t1, t2) :: ts) =
        if i < size t1 then TWO (updateTree (i, y, t1), t2) :: ts
        else if (i - size t1) < size t2 then TWO (t1, updateTree (i - size t1, y, t2)) :: ts
        else TWO (t1, t2) :: update (i - size t1 - size t2, y, ts)
    | update (i, y, THREE (t1, t2, t3) :: ts) =
        if i < size t1 then THREE (updateTree (i, y, t1), t2, t3) :: ts
        else if (i - size t1) < size t2 then THREE (t1, updateTree (i - size t1, y, t2), t3) :: ts
        else if (i - size t1 - size t2) < size t3 then THREE (t1, t2, updateTree (i - size t1 - size t2, y, t3)) :: ts
        else THREE (t1, t2, t3) :: update (i - size t1 - size t2 - size t3, y, ts)
end

val t = foldl ZerolessRedundantBinaryRandomAccessList.cons ZerolessRedundantBinaryRandomAccessList.empty [1,2,3,4,5]
val e0 = ZerolessRedundantBinaryRandomAccessList.lookup (0, t)
val e1 = ZerolessRedundantBinaryRandomAccessList.lookup (1, t)
val e2 = ZerolessRedundantBinaryRandomAccessList.lookup (2, t)
val e3 = ZerolessRedundantBinaryRandomAccessList.lookup (3, t)
val e4 = ZerolessRedundantBinaryRandomAccessList.lookup (4, t)


