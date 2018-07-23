use "QUEUE.sml";

Control.lazysml := true;
open Lazy
fun force ($ x) = x

functor YABootsrappedQueue (PrimQ : QUEUE) : QUEUE = 
struct
  datatype 'a Queue = E | Q of int * 'a list * 'a list susp PrimQ.Queue * int * 'a list

  val empty = E

  fun isEmpty E = true
    | isEmpty _ = false

  fun checkQ (lenfm, f, m, lenr, r) =
    if lenr <= lenfm
    then checkF(lenfm, f, m, lenr, r)
    else checkF(lenfm + lenr, f, PrimQ.snoc(m, $(rev(r))), 0, [])

  and checkF (lenfm, [], m, lenr, r) =
      if PrimQ.isEmpty(m)
      then E else Q (lenfm, force(PrimQ.head(m)), PrimQ.tail(m), lenr, r)
    | checkF (q) = Q (q)

  and snoc (E, x) = Q (1, [x], PrimQ.empty, 0, [])
    | snoc (Q (lenfm, f, m, lenr, r), x) = checkQ(lenfm, f, m, lenr+1, x::r)

  and head E = raise Empty
    | head (Q (lenfm, x::f', m, lenr, r)) = x

  and tail E = raise Empty
    | tail (Q (lenfm, x::f', m, lenr, r)) = checkQ(lenfm-1, f', m, lenr, r)
end

