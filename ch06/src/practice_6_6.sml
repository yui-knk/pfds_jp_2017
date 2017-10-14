(* http://d.hatena.ne.jp/eldesh/20110704/1309759499 *)
(* https://www.cs.cmu.edu/~rwh/introsml/core/lazydata.htm *)

Control.lazysml := true;
open Lazy

fun force ($ x) = x

datatype 'a Queue = Queue of 'a list * int * 'a list susp * int * 'a list

val empty = Queue ([], 0, $[], 0, [])
 
fun checkw (Queue ([], lenf, f, lenr, r)) = Queue (force f, lenf, f, lenr, r)
  | checkw q = q

fun check (q as (Queue (w, lenf, f, lenr, r))) =
  if lenr <= lenf then checkw q
  else checkw (Queue (w, lenf + lenr, $((force f) @ (rev r)), 0, []))

fun snoc (Queue (w, lenf, f, lenr, r), x) = check (Queue (w, lenf, f, lenr + 1, x :: r))

fun head (Queue ([], lenf, f, lenr, r)) = raise Empty
  | head (Queue (x::w, lenf, f, lenr, r)) = x

fun tail (Queue ([], lenf, f, lenr, r)) = raise Empty
  | tail (Queue (x::w, lenf, f, lenr, r)) = check (Queue (w, lenf - 1, $(tl (force f)), lenr, r))

val f  = $[1]
val r  = [3,2]
val r2 = [5,4]
val l1 = $((force f) @ (rev r));
val l2 = $((force l1) @ (rev r2));

val a = (snoc (empty, 1));
val b = (snoc (snoc (snoc (empty, 1), 2), 3));
val c = (snoc (snoc (snoc (snoc (snoc (snoc (snoc (empty, 1), 2), 3), 4), 5), 6), 7));
val d = tail c;

(*stream*)

datatype 'a StreamCell = NIL | CONS of 'a * 'a Stream
withtype 'a Stream = 'a StreamCell susp

val s = $(CONS(2, $(CONS(1, $NIL))))
