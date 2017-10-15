Control.lazysml := true;
open Lazy

fun force ($ x) = x

signature STREAM =
sig
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  val concat : 'a Stream * 'a Stream -> 'a Stream
  val n_concat : 'a Stream * 'a Stream -> 'a Stream
  val take   : int * 'a Stream -> 'a Stream
end

structure Stream : STREAM = 
struct
  datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
  withtype 'a Stream = 'a StreamCell susp

  fun lazy concat (($NIL), t) = t
         | concat (($(CONS (x, s))), t) = $(CONS (x, concat (s, t)))

  fun n_concat (($NIL), t) = t
    | n_concat (($(CONS (x, s))), t) = $(CONS (x, concat (s, t)))

  fun lazy take (0, s) = $NIL
         | take (n, $NIL) = $NIL
         | take (n, $(CONS (x, s))) = $(CONS (x, take (n - 1, s)))
end

val a = Stream.concat ($(Stream.CONS(1, $Stream.NIL)), $(Stream.CONS(2, $Stream.NIL)));
val b = Stream.n_concat ($(Stream.CONS(1, $Stream.NIL)), $(Stream.CONS(2, $Stream.NIL)));
