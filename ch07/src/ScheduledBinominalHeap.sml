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

structure IntOrdered : ORDERED =
struct
  type T = int

  fun eq  (a, b) = (a = b)
  fun lt  (a, b) = (a < b)
  fun leq (a, b) = (a <= b)
end

signature HEAP = 
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
(*  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap*)
end


functor ScheduledBinominalHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = NODE of Elem.T * Tree list
  datatype Digit = ZERO | ONE of Tree
  type Schedule = Digit Stream list
  type Heap = Digit Stream * Schedule

  val empty = ($NIL, [])
  fun isEmpty ($NIL, _) = true | isEmpty _ = false

  fun link (t1 as NODE(x1, c1), t2 as NODE(x2, c2)) =
    if Elem.leq (x1, x2) then NODE(x1, t2 :: c1) else NODE(x2, t1 :: c2)
  (* insTree only handle Digit Stream *)
  fun lazy insTree (t, $NIL) = $(CONS(ONE t, $NIL))
    | insTree (t, $(CONS(ZERO, ds))) = $(CONS(ONE t, ds))
    | insTree (t, $(CONS(ONE t', ds))) = $(CONS(ZERO, insTree (link (t, t'), ds)))

  fun exec [] = []
    | exec ($(CONS(ZERO, job)) :: sched) = job :: sched
    | exec (_ :: sched) = sched
  fun insert (x, (ds, sched)) =
    let val ds' = insTree (NODE(x, []), ds)
    in (ds', exec(exec(ds' :: sched))) end
end

structure IntHeap = ScheduledBinominalHeap (IntOrdered)

val a = IntHeap.empty;
val b = IntHeap.insert(1, a);
val c = IntHeap.insert(2, b);
val d = IntHeap.insert(3, c);

val e = foldl IntHeap.insert IntHeap.empty [1,2,3,4,5,6,7,8];
