Control.lazysml := true;
open Lazy

fun force ($ x) = x

datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
withtype 'a Stream = 'a StreamCell susp

fun lazy ($NIL) :: t = t
       | ($(CONS (x, s))) :: t = $(CONS (x, s :: t))

fun lazy reverse s =
  let fun reverse' ($NIL, r) = r
        | reverse' ($(CONS (x, s)), r) = reverse' (s, $(CONS (x, r)))
  in reverse' (s, $NIL) end

signature QUEUE = 
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

structure BankersQueue : QUEUE =
struct
  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $NIL, 0, $NIL)
  fun isEmpty (lenf, _, _, _) = (lenf = 0)

  fun rotate ($NIL, $(CONS (y, _)), a) = $(CONS(y, a))
    | rotate ($(CONS (x, xs)), $(CONS (y, ys)), a) =
        $(CONS (x, rotate (xs, ys, $(CONS(y, a)))))

  fun check (q as (lenf, f, lenr, r)) =
    if lenr <= lenf then q else (lenf + lenr, rotate (f, r, $NIL), 0, $NIL)

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr + 1, $(CONS (x, r)))

  fun head (lenf, $NIL, lenr, r) = raise Empty
    | head (lenf, $(CONS (x, f')), lenr, r) = x

  fun tail (lenf, $NIL, lenr, r) = raise Empty
    | tail (lenf, $(CONS (x, f')), lenr, r) = check (lenf - 1, f', lenr, r)
end

val a = BankersQueue.snoc(BankersQueue.snoc(BankersQueue.empty, 1), 2)
val b = BankersQueue.snoc(BankersQueue.snoc(a, 3), 4)
val c = BankersQueue.tail(b)
val d = BankersQueue.head(b)
val e = BankersQueue.tail(BankersQueue.tail(b))
val f = BankersQueue.head(BankersQueue.tail(b))
