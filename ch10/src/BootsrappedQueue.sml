use "QUEUE.sml";

Control.lazysml := true;
open Lazy
fun force ($ x) = x

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
    else checkF(lenfm + lenr, f, snocEL(m, LIST ($(rev(r)))), 0, [])

  and checkF (lenfm, [], E, lenr, r) = E
    | checkF (lenfm, [], m, lenr, r) = Q (lenfm, headEL(m), tail(m), lenr, r)
    | checkF (q) = Q (q)

  (* 'a Queue -> 'a EL -> 'a Queue *)
  and snocEL (E, el) = Q (1, [el], E, 0, [])
    | snocEL (Q (lenfm, f, m, lenr, r), el) = checkQ(lenfm, f, m, lenr+1, el::r)

  and snoc (E, x) = Q (1, [ELEM(x)], E, 0, [])
    | snoc (Q (lenfm, f, m, lenr, r), x) = checkQ(lenfm, f, m, lenr+1, ELEM(x)::r)

  (* 'a Queue -> 'a EL *)
  and headEL E = raise Empty
    | headEL (Q (lenfm, LIST(x)::f', m, lenr, r)) = force x

  and head E = raise Empty
    | head (Q (lenfm, ELEM(x)::f', m, lenr, r)) = x

  and tail E = raise Empty
    | tail (Q (lenfm, x::f', m, lenr, r)) = checkQ(lenfm-1, f', m, lenr, r)
end

fun flip f (x, y) = f (y ,x)

val q1 = foldl (flip BootsrappedQueue.snoc) BootsrappedQueue.empty [1,2,3,4,5,6];
val q2 = BootsrappedQueue.tail q1;
val q3 = BootsrappedQueue.tail q2;
val q4 = BootsrappedQueue.tail q3;
val q5 = BootsrappedQueue.tail q4;
