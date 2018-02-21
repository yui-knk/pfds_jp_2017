use "RANDOMACCESSLIST.sml";

structure BinaryRandomAccessList : RANDOMACCESSLIST =
struct
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  datatype 'a Digit = ZERO | ONE of 'a Tree
  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)

  fun consTree (t, []) = [ONE t]
    | consTree (t, ZERO :: ts) = ONE t :: ts
    | consTree (t1, ONE t2 :: ts) = ZERO :: consTree (link (t1, t2), ts)

  (*
    例えば val t1 = foldl BinaryRandomAccessList.cons BinaryRandomAccessList.empty [1,2,3,4] のとき
    BinaryRandomAccessList.unconsTree t1 は
    (LEAF 4,[ONE (LEAF 3),ONE (NODE (#,#,#))])
  *)
  fun unconsTree [] = raise Empty
    | unconsTree [ONE t] = (t, [])
    | unconsTree (ONE t :: ts) = (t, ZERO :: ts)
    | unconsTree (ZERO :: ts) =
        (*
          先頭がZEROにmatchしているので、tsの最初のONEは
          必ずrankが1以上であり、そのようなTreeはNODEからはじまる。
        *)
        let val (NODE (_, t1, t2), ts') = unconsTree ts
        in (t1, ONE t2 :: ts') end

  fun cons (x, ts) = consTree (LEAF x, ts)
  fun head ts = let val (LEAF x, _) = unconsTree ts in x end
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

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
    | lookup (i, ZERO :: ts) = lookup (i, ts)
    | lookup (i, ONE t :: ts) =
        if i < size t then lookupTree (i, t) else lookup (i - size t, ts)

  fun update (i, y, []) = raise Subscript
    | update (i, y, ZERO :: ts) = ZERO :: update (i, y, ts)
    | update (i, y, ONE t :: ts) =
        if i < size t then ONE (updateTree (i, y, t)) :: ts
        else ONE t :: update (i - size t, y, ts)
end

val t1 = foldl BinaryRandomAccessList.cons BinaryRandomAccessList.empty [1,2,3,4]
val e1 = BinaryRandomAccessList.lookup (1, t1)
