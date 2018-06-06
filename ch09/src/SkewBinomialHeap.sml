use "HEAP.sml";
use "ORDERED.sml";

functor SkewBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element
  datatype Tree = NODE of int * Elem.T * Elem.T list * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (NODE (r, x, xs, c)) = r
  fun root (NODE (r, x, xs, c)) = x

  fun link (t1 as NODE (r, x1, xs1, c1), t2 as NODE (_, x2, xs2, c2)) =
        if Elem.leq (x1, x2) then NODE (r+1, x1, xs1, t2::c1)
        else NODE (r+1, x2, xs2, t1::c2)

  fun skewLink (x, t1, t2) =
        let val NODE (r, y, ys, c) = link (t1, t2)
        in
            if Elem.leq (x, y) then NODE (r, x, y::ys, c)
            else NODE (r, y, x::ys, c)
        end

  fun insTree (t, []) = [t]
    | insTree (t1, t2::ts) =
        if rank t1 < rank t2 then t1::t2::ts
        else insTree (link (t1, t2), ts)

  fun mergeTrees (ts1, []) = ts1
    | mergeTrees ([], ts2) = ts2
    | mergeTrees (ts1 as t1::ts1', ts2 as t2::ts2') =
        if rank t1 < rank t2 then t1 :: mergeTrees (ts1', ts2)
        else if rank t1 > rank t2 then t2 :: mergeTrees (ts1, ts2')
        else insTree (link (t1, t2), mergeTrees (ts1', ts2'))

  fun normalize [] = []
    | normalize (t::ts) = insTree (t, ts)

  fun insert (x, ts as t1::t2::rest) =
        if rank t1 = rank t2 then skewLink (x, t1 ,t2) :: rest
        else NODE (0, x, [], []) :: ts
    | insert (x, ts) = NODE (0, x, [], []) :: ts

  fun merge (ts1, ts2) = mergeTrees (normalize ts1, normalize ts2)

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t::ts) =
        let val (t', ts') = removeMinTree (ts)
        in if Elem.leq (root t, root t') then (t, ts) else (t', t::ts') end

  fun findMin ts =
        let val (t, _) = removeMinTree ts in root t end

  fun deleteMin ts =
        let val (NODE (_, x, xs, ts1), ts2) = removeMinTree ts
            fun insertAll ([], ts) = ts
              | insertAll (x::xs, ts) = insertAll (xs, insert (x, ts))
        in insertAll (xs, merge (rev ts1, ts2)) end
end


structure IntSort = SkewBinomialHeap (IntOrdered)
