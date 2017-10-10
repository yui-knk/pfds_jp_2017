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
  else let val f' = force f
    in checkw (Queue (f', lenf + lenr, $(f' @ rev r), 0, [])) end

fun snoc (Queue (w, lenf, f, lenr, r), x) = Queue (w, lenf, f, lenr + 1, x :: r)

fun head (Queue ([], lenf, f, lenr, r)) = raise Empty
  | head (Queue (x::w, lenf, f, lenr, r)) = x

fun tail (Queue ([], lenf, f, lenr, r)) = raise Empty
  | tail (Queue (x::w, lenf, f, lenr, r)) = check (Queue (w, lenf - 1, $(tl (force f)), lenr, r))

