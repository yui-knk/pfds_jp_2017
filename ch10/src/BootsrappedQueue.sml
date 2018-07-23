use "QUEUE.sml";

Control.lazysml := true;
open Lazy

structure BootsrappedQueue : QUEUE =
struct
  datatype 'a EL = ELEM of 'a | LIST of 'a EL list susp
  datatype 'a Queue = E | Q of int * 'a EL list * 'a Queue * int * 'a EL list

  val empty = E

  fun isEmpty E = true
    | isEmpty _ = false

  fun checkQ (lenfm, f, m, lenr, r) =
    if lenr <= lenfm
    then checkF(lenfm, f, m, lenr, r)
    else checkF(lenfm + lenr, f, snoc(m, $(rev(r))), 0, [])

  and checkF (lenfm, [], E, lenr, r) = E
    | checkF (lenfm, [], m, lenr, r) = Q (lenfm, head(m), tail(m), lenr, r)
    | checkF (q) = Q (q)

  and snoc (E, x) = Q (1, [ELEM(x)], E, 0, [])
    | snoc (Q (lenfm, f, m, lenr, r), x) = checkQ(lenfm, f, m, lenr+1, ELEM(x)::r)

  and head E = raise Empty
    | head (Q (lenfm, x::f', m, lenr, r)) = x

  and tail E = raise Empty
    | tail (Q (lenfm, x::f', m, lenr, r)) = checkQ(lenfm-1, f', m, lenr, r)
end