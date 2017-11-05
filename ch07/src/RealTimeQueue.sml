Control.lazysml := true;
open Lazy

fun force ($ x) = x

datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
withtype 'a Stream = 'a StreamCell susp

signature QUEUE = 
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

structure RealTimeQueue : QUEUE =
struct
  type 'a Queue = 'a Stream * 'a list * 'a Stream

  val empty = ($NIL, [], $NIL)
  fun isEmpty ($NIL, _, _) = true
    | isEmpty _ = false

  fun rotate ($NIL, y :: _, a) = $(CONS(y, a))
    | rotate ($(CONS (x, xs)), y :: ys, a) =
        $(CONS (x, rotate (xs, ys, $(CONS(y, a)))))

  fun exec (f, r, $(CONS(x, s))) = (f, r, s)
    | exec (f, r, $NIL) = let val f' = rotate (f, r, $NIL) in (f', [], f') end

  fun snoc ((f, r, s), x) = exec (f, x :: r, s)

  fun head ($NIL, r, s) = raise Empty
    | head ($(CONS (x, f)), r, s) = x

  fun tail ($NIL, r, s) = raise Empty
    | tail ($(CONS (x, f)), r, s) = exec (f, r, s)
end

val a = RealTimeQueue.snoc(RealTimeQueue.snoc(RealTimeQueue.empty, 1), 2)
val b = RealTimeQueue.snoc(RealTimeQueue.snoc(a, 3), 4)
val c = RealTimeQueue.tail(b)
val d = RealTimeQueue.head(b)
val e = RealTimeQueue.tail(RealTimeQueue.tail(b))
val f = RealTimeQueue.head(RealTimeQueue.tail(b))
val g = RealTimeQueue.tail(RealTimeQueue.tail(RealTimeQueue.tail(b)))
val h = RealTimeQueue.head(RealTimeQueue.tail(RealTimeQueue.tail(b)))
