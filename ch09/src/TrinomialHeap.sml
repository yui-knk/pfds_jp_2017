use "ORDERED.sml";
use "HEAP.sml";

functor TrinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element
  datatype Tree = NODE of Elem.T * (Tree * Tree) list
  datatype Digit = ZERO | ONE of Tree | TWO of Tree * Tree
  type Heap = Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun root (NODE (x, ts)) = x

  fun link (t1 as NODE(x1, c1), t2 as NODE(x2, c2), t3 as NODE(x3, c3)) =
        let val y1::y2::y3::_ = ListMergeSort.sort (fn (x, y) => Elem.lt(y, x)) [x1, x2, x3]
        in
          if      Elem.eq(y1, x1) then NODE (x1, (t2, t3)::c1)
          else if Elem.eq(y1, x2) then NODE (x2, (t1, t3)::c2)
          else                         NODE (x3, (t1, t2)::c3)
        end

  fun insTree (t, []) = [ONE (t)]
    | insTree (t, ZERO :: ds) = ONE (t) :: ds
    | insTree (t, ONE (t1) :: ds) = TWO (t, t1) :: ds
    | insTree (t, TWO (t1, t2) :: ds) = ZERO :: (insTree (link (t, t1, t2), ds))

  fun insert (x, ds) = insTree (NODE (x, []), ds)

  fun mrg (ds1, []) = ds1
    | mrg ([], ds2) = ds2
    | mrg (ZERO :: ds1, d2 :: ds2) = d2 :: mrg (ds1, ds2)
    | mrg (d1 :: ds1, ZERO :: ds2) = d1 :: mrg (ds1, ds2)
    | mrg (ONE (t1) :: ds1, ONE (t2) :: ds2) = TWO (t1, t2) :: mrg (ds1, ds2)
    | mrg (ONE (t1) :: ds1, TWO (a, b) :: ds2) = ZERO :: mrg (insTree (link (t1, a, b), ds1), ds2)
    | mrg (TWO (a, b) :: ds1, ONE (t2) :: ds2) = ZERO :: mrg (insTree (link (t2, a, b), ds1), ds2)
    | mrg (TWO (a1, b1) :: ds1, TWO (a2, b2) :: ds2) = ONE (a1) :: ONE (link (b1, a2, b2)) :: mrg (ds1, ds2)

  fun merge (ds1, ds2) = mrg (ds1, ds2)

  fun removeMinTree ([]) = raise Empty
    | removeMinTree (ZERO :: ds) =
        let val (t', ds') = removeMinTree ds in (t', ZERO :: ds') end
    | removeMinTree [ONE (t)] = (t, [])
    | removeMinTree (ONE (t) :: ds) =
        let val (t', ds') = removeMinTree ds
        in
          if Elem.leq (root t, root t')
          then
            (t, ZERO :: ds)
          else
            (t', insTree (t, ds'))
        end
    | removeMinTree ([TWO (a, b)]) =
        if Elem.leq (root a, root b) then (a, [ONE (b)]) else (b, [ONE (a)])
    | removeMinTree (TWO (a, b) :: ds) =
        let
          val (l, g) = if Elem.leq (root a, root b) then (a, b) else (b, a)
          val (t', ds') = removeMinTree (ds)
        in
          if Elem.leq (root l, root t')
          then
            (l, ONE (g) :: ds)
          else
            (t', TWO (a, b) :: ds')
        end

  fun findMin (ds) =
        let val (NODE (x, _), _) = removeMinTree ds in x end

  fun deleteMin (ds) =
        let
          val (NODE (_, c), li) = removeMinTree ds
          val r = foldr (fn ((a, b), l) => TWO (a, b) :: l) [] c
        in merge (r, li) end
end

structure TI = TrinomialHeap (IntOrdered)

val h1 = foldl TI.insert TI.empty [1,2,3,4];
val h2 = foldl TI.insert TI.empty [5,6,7,8,9];
val h3 = TI.merge (h1, h2);
val h4 = TI.merge (h1, h1);
val h5 = TI.deleteMin (TI.deleteMin h2);;
