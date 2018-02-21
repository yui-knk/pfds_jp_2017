use "RANDOMACCESSLIST.sml";

signature RANDOMACCESSLISWITHDROP =
sig
  include RANDOMACCESSLIST

  val drop : int * 'a RList -> 'a RList
end

structure BinaryRandomAccessList : RANDOMACCESSLISWITHDROP =
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

  (*
    [ZERO, ZERO] のようにならないときだけ、
    ZERO を追加する。
  *)
  fun consZero ([]) = []
    | consZero (ts) = ZERO :: ts

  (*
    停止条件は LEAF に触ること。
  *)
  fun fillWithZero (LEAF x, ts) = ts
    | fillWithZero (NODE (w, t1, t2), ts) = fillWithZero (t1, consZero (ts))

  (*
    停止条件は k = 0 になること、もしくは
    LEAF に触ること。
    k = 0 になったときに、NODE の場合は、
    それより低い位置を ZERO で埋めるために fillWithZero を呼び出す。

    O(log n) で処理をするために、常に NODE のどちらかの Tree にのみ
    操作を行うことができる。
    LEAF に向かいながら、上位の桁から決定していく。

    `k < w div 2` および `k = w div 2` のときは右の Tree をその後の操作で触ることがないため
    右の Tree を上位の桁として決定してよい。

    `k > w div 2` のときは今現在の桁にはbitが立たないため、 ZERO 上位の桁として決定してよい。
  *)
  fun dropTree (k, LEAF x, ts) = ts
    | dropTree (k, NODE (w, t1, t2), ts) =
        if k = w div 2 then fillWithZero (t1, ONE t2 :: ts)
        else if k < w div 2 then dropTree (k, t1, ONE t2 :: ts)
        else dropTree (k - w div 2, t2, consZero (ts))

  fun drop (k, []) = raise Subscript
    | drop (0, ts) = ts
    | drop (k, ZERO :: ts) = drop (k, ts)
    | drop (k, ONE t :: ts) =
        if k <= size t then dropTree (k, t, ts)
        else drop (k - size t, ts)
end

val t = foldl BinaryRandomAccessList.cons BinaryRandomAccessList.empty [1,2,3,4]
val e1 = BinaryRandomAccessList.lookup (1, t)

val t4 = BinaryRandomAccessList.drop (0, t)
val t3 = BinaryRandomAccessList.drop (1, t)
val t2 = BinaryRandomAccessList.drop (2, t)
val t1 = BinaryRandomAccessList.drop (3, t)
val t0 = BinaryRandomAccessList.drop (4, t)
