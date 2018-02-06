Control.lazysml := true;
open Lazy

fun force ($ x) = x

signature STREAM =
sig
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  val concat  : 'a Stream * 'a Stream -> 'a Stream
  val take    : int * 'a Stream -> 'a Stream
  val drop    : int * 'a Stream -> 'a Stream
  val reverse : 'a Stream -> 'a Stream
end

structure Stream : STREAM = 
struct
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  fun lazy concat (($NIL), t) = t
         | concat (($(CONS (x, s))), t) = $(CONS (x, concat (s, t)))

  fun lazy take (0, s) = $NIL
         | take (n, $NIL) = $NIL
         | take (n, $(CONS (x, s))) = $(CONS (x, take (n - 1, s)))

  fun lazy drop (n, s) =
      let fun drop' (0, s) = s
            | drop' (n, $NIL) = $NIL
            | drop' (n, $(CONS(x, s))) = drop' (n - 1, s)
      in drop'(n, s) end

  fun lazy reverse r =
      let fun reverse' ($NIL, r) = r
            | reverse' ($(CONS(x, s)), r) = reverse' (s, $(CONS(x, r)))
      in reverse' (r, $NIL) end
end

signature DEQUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val cons    : 'a * 'a Queue -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue

  val snoc    : 'a Queue * 'a -> 'a Queue
  val last    : 'a Queue -> 'a
  val init    : 'a Queue -> 'a Queue
end

functor BankersDeque (val c : int) : DEQUE =
struct
  open Stream

  type 'a Queue = int * 'a Stream * int * 'a Stream

  val empty = (0, $NIL, 0, $NIL)
  fun isEmpty (lenf, f, lenr, r) = (lenf + lenr = 0)

  fun check (q as (lenf, f, lenr, r)) =
       if lenf > c * lenr + 1 then
         let val i = (lenf + lenr) div 2
             val j = lenf + lenr - i
             val f' = take (i, f)
             val r' = concat (r, reverse (drop (i, f)))
         in (i, f', j, r') end
       else if lenr > c * lenf + 1 then
         let val j = (lenf + lenr) div 2
             val i = lenf + lenr - j
             val r' = take (j, r)
             val f' = concat(f, reverse (drop (j, r)))
          in (i, f', j, r') end
       else q

  fun cons (x, (lenf, f, lenr, r)) = check (lenf + 1, $(CONS(x, f)), lenr, r)
  fun head (lenf, $NIL, lenr, $NIL) = raise Empty
    | head (lenf, $NIL, lenr, $(CONS(x, _))) = x
    | head (lenf, $(CONS(x, f')), lenr, r) = x
  fun tail (lenf, $NIL, lenr, $NIL) = raise Empty
    | tail (lenf, $NIL, lenr, $(CONS(x, _))) = empty
    | tail (lenf, $(CONS(x, f')), lenr, r) = check (lenf - 1, f', lenr, r)

  fun snoc ((lenf, f, lenr, r), x) = check (lenf, f, lenr + 1, $(CONS(x, r)))
  fun last (lenf, $NIL, lenr, $NIL) = raise Empty
    | last (lenf, $(CONS(x, _)), lenr, $NIL) = x
    | last (lenf, f, lenr, $(CONS(x, _))) = x
  fun init (lenf, $NIL, lenr, $NIL) = raise Empty
    | init (lenf, $(CONS(x, _)), lenr, $NIL) = empty
    | init (lenf, f, lenr, $(CONS(x, r'))) = check (lenf, f, lenr - 1, r')
end


