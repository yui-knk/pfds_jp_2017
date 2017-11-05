Control.lazysml := true;
open Lazy

fun force ($ x) = x

signature ORDERED = 
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

signature SORTABLE = 
sig
  structure Elem : ORDERED

  type Sortable

  val empty : Sortable
  val add   : Elem.T * Sortable -> Sortable
  val sort  : Sortable -> Elem.T list
end

functor BottomUpMergeSort (Element : ORDERED) : SORTABLE = 
struct
  structure Elem = Element

  type Sortable = int * Elem.T list list susp

  fun mrg ([], ys) = ys
    | mrg (xs, []) = xs
    | mrg (xs as x :: xs', ys as y :: ys') =
        if Elem.leq (x, y) then x :: mrg (xs', ys) else y :: mrg (xs, ys')

  val empty = (0, $[])

  fun add (x, (size, segs)) =
       let fun addSeg (seg, segs, size) = 
                if size mod 2 = 0 then seg :: segs
                else addSeg (mrg (seg, hd segs), tl segs, size div 2)
       in (size + 1, $(addSeg ([x], force segs, size))) end

  fun sort (size, segs) =
       let fun mrgAll (xs, []) = xs
             | mrgAll (xs, seg :: segs) = mrgAll (mrg (xs, seg), segs)
       in mrgAll ([], force segs) end
end

structure IntOrdered : ORDERED =
struct
  type T = int

  fun eq  (a, b) = (a = b)
  fun lt  (a, b) = (a < b)
  fun leq (a, b) = (a <= b)
end

structure IntSort = BottomUpMergeSort (IntOrdered)

val a = IntSort.add(4, IntSort.add(2, IntSort.add (3, IntSort.add (1, IntSort.empty))));
val b = IntSort.sort(a);
