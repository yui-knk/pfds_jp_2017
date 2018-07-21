use "HEAP.sml";
use "ORDERED.sml";
use "SkewBinomialHeap.sml";

signature HEAPEXTENDED =
sig
  include HEAP

  val delete : Elem.T * Heap -> Heap
end


functor DeletableHeap (H : HEAP) : HEAPEXTENDED =
struct
  structure Elem = H.Elem
  (*
    左のheapを正の、右のheapを負のheapとする。
    正のheapの最小要素は、負のheapの最小要素より真に小さいとする。
    そのため、heapからの物理的な削除はinsertやdelete, deleteMin関数実行時に行う。
  *)
  type Heap = H.Heap * H.Heap

  val empty = (H.empty, H.empty)
  fun isEmpty (ph, mh) = H.isEmpty ph

  fun check (ph, mh) =
        if (H.isEmpty ph orelse H.isEmpty mh) then (ph, mh)
        else if Elem.eq (H.findMin ph, H.findMin mh) then check (H.deleteMin ph, H.deleteMin mh)
        else (ph, mh)

  fun insert (x, (ph, mh)) = check (H.insert (x, ph), mh)
  fun merge ((ph1, mh1), (ph2, mh2)) = (H.merge (ph1, ph2), H.merge (mh1, mh2))

  fun findMin (ph, mh) = H.findMin ph
  fun deleteMin (ph, mh) = check (H.deleteMin ph, mh)

  fun delete (x, (ph, mh)) =
        if Elem.eq (H.findMin ph, x) then check (H.deleteMin ph, mh)
        else if Elem.lt (x, H.findMin ph) then raise Subscript
        else (ph, H.insert (x, mh))
end

structure DeletableIntHeap = DeletableHeap (IntSkewBinomialHeap)
structure D = DeletableIntHeap

val h1 = foldl D.insert D.empty [1,2,3,4];
val h2 = foldl D.delete h1 [1,3];
val h3 = D.deleteMin h2;

