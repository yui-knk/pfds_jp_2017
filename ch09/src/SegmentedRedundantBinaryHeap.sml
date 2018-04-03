use "ORDERED.sml";
use "HEAP.sml";

functor SegmentedRedundantBinaryHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = NODE of Elem.T * Tree list
  datatype Digit = ZERO | ONES of Tree list | TWO of Tree * Tree
  type Heap = Digit list

  fun root (NODE (x, li)) = x

  val empty = []
  fun isEmpty ds = null ds

  fun link (t1 as NODE(x1, c1), t2 as NODE(x2, c2)) =
    if Elem.leq (x1, x2) then NODE(x1, t2 :: c1) else NODE(x2, t1 :: c2)

  fun ones ([], ds)              = ds
    | ones (ts, ONES (os) :: ds) = ONES (ts@os) :: ds
    | ones (ts, ds)              = ONES (ts) :: ds

  fun simpleInsTree (t, [])                  = [ONES [t]]
    | simpleInsTree (t, ZERO :: ds)          = ones ([t], ds)
    | simpleInsTree (t, ONES (hd::tl) :: ds) = TWO (t, hd) :: ones (tl, ds)

  fun fixup (TWO (a, b) :: ds)            = ZERO :: simpleInsTree (link (a, b), ds)
    | fixup (ONES ts :: TWO (a, b) :: ds) = ONES ts :: ZERO :: simpleInsTree (link (a, b), ds)
    | fixup (ds) = ds

  fun insTree (t, ds) = fixup (simpleInsTree (t, ds))

  fun insert (x, ds) = insTree (NODE (x, []), ds)

  fun mrg (ds1, []) = ds1
    | mrg ([], ds2) = ds2
    | mrg (ZERO :: ds1, d2 :: ds2) = fixup (d2 :: mrg (ds1, ds2))
    | mrg (d1 :: ds1, ZERO :: ds2) = fixup (d1 :: mrg (ds1, ds2))
    | mrg (TWO (a, b) :: ds1, d2 :: ds2) = fixup (d2 :: mrg (simpleInsTree (link (a, b), ds1), ds2))
    | mrg (d1 :: ds1, TWO (a, b) :: ds2) = fixup (d1 :: mrg (simpleInsTree (link (a, b), ds2), ds1))
    | mrg (ONES (h1::t1) :: ds1, ONES (h2::t2) :: ds2) = fixup (TWO (h1, h2) :: mrg (ones (t1, ds1), ones (t2, ds2)))

  fun merge (ds1, ds2) = mrg (ds1, ds2)

  fun removeMinTree ([]) = raise Empty
    | removeMinTree (ZERO :: ds) =
        let val (t', ds') = removeMinTree ds in (t', ZERO :: ds') end
    | removeMinTree ([ONES [hd]]) = (hd, [])
    | removeMinTree (ONES [hd] :: ds) =
        let val (t', ds') = removeMinTree ds
        in
          if Elem.leq (root hd, root t')
          then
            (hd, ZERO :: ds)
          else
            (t', ones ([hd], ds'))
        end
    | removeMinTree (ONES (hd::tl) :: ds) =
        let val (t', ds') = removeMinTree (ones (tl, ds))
        in
          if Elem.leq (root hd, root t')
          then
            (hd, ZERO :: ones (tl, ds))
          else
            (t', ones ([hd], ds'))
        end
    | removeMinTree ([TWO (a, b)]) =
        if Elem.leq (root a, root b) then (a, [ONES [b]]) else (b, [ONES [a]])
    | removeMinTree (TWO (a, b) :: ds) =
        let
          val (l, g) = if Elem.leq (root a, root b) then (a, b) else (b, a)
          val (t', ds') = removeMinTree (ds)
        in
          if Elem.leq (root l, root t')
          then
            (l, ones ([g], ds))
          else
            (t', fixup (TWO (a, b) :: ds'))
        end

  fun findMin (ds) =
    let val (NODE (x, _), _) = removeMinTree ds in x end

  fun deleteMin (ds) =
    let
      val (NODE (_, c), li) = removeMinTree ds
      val r = foldr (fn (e, l) => ones ([e], l)) [] c
    in merge (r, li) end
end
structure IntHeap = SegmentedRedundantBinaryHeap (IntOrdered)

structure Util =
struct
  open IntHeap

  fun numList_ 0 acc = acc
    | numList_ i acc = numList_ (i - 1) (i :: acc)

  fun numList i = numList_ i []

  fun heapList_ 0 acc = acc
    | heapList_ i acc =
      let
        val zeroOne = foldl insert empty (numList i)
        val t = (i, zeroOne)
      in
        heapList_ (i - 1) (t :: acc)
      end

  fun heapList i = heapList_ i []

  fun heap i = foldl insert empty (numList i)

  fun deleteMinWhile i heap =
    foldl (fn (_, (acc, h)) => ((findMin h)::acc, deleteMin h)) ([], heap) (numList i)

end

val h1 = IntHeap.merge (Util.heap 10, Util.heap 10)
val h2 = IntHeap.deleteMin h1
val h3 = IntHeap.deleteMin h2

val e1 = IntHeap.findMin h1
val e2 = IntHeap.findMin h2
val e3 = IntHeap.findMin h3

val (l1, h) = Util.deleteMinWhile 15 h1
