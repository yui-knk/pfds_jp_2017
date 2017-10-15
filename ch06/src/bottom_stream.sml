Control.lazysml := true;
open Lazy

fun force ($ x) = x

datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
withtype 'a Stream = 'a StreamCell susp

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
  val sort  : Sortable -> Elem.T Stream
end

functor BottomUpMergeSort (Element : ORDERED) : SORTABLE = 
struct
  structure Elem = Element

  type Sortable = int * Elem.T Stream list

  (* 'a StreamCell * 'a StreamCell -> 'a StreamCell *)
  fun mrg (NIL, ys) = ys
    | mrg (xs, NIL) = xs
    | mrg (xs as CONS(x, xs'), ys as CONS(y, ys')) =
        if Elem.leq (x, y) then (CONS(x, $(mrg (force xs', ys)))) else (CONS(y, $(mrg (xs, force ys'))))

  val empty = (0, [])

  fun add (x, (size, segs)) =
       (*
          ここではsegとsegsをそのまま結合したいため、
          'a Stream
          'a Stream list
          であつかう。
        *)
       let fun addSeg (seg, segs, size) = 
                if size mod 2 = 0 then seg :: segs
                else addSeg ($(mrg (force seg, force (hd segs))), tl segs, size div 2)
       in (size + 1, addSeg ($(CONS(x, $NIL)), segs, size)) end

  fun sort (size, segs) =
       let fun mrgAll (xs, []) = $(xs)
             | mrgAll (xs, seg :: segs) = mrgAll (mrg (xs, force seg), segs)
       in mrgAll (NIL, segs) end
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
