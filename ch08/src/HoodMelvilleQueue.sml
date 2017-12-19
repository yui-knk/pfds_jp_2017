signature QUEUE = 
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

structure HoodMelvilleQueue : QUEUE =
struct
  datatype 'a RotationState =
      IDLE
    | REVERSING of int * 'a list * 'a list * 'a list * 'a list
    | APPENDING of int * 'a list * 'a list
    | DONE of 'a list

  type 'a Queue = int * 'a list * 'a RotationState * int * 'a list

  fun exec (REVERSING (ok, x::f, f', y::r, r')) = REVERSING (ok+1, f, x::f', r, y::r')
    | exec (REVERSING (ok, [], f', [y], r')) = APPENDING (ok, f', y::r')
    | exec (APPENDING (0, f', r')) = DONE r'
    | exec (APPENDING (ok, x::f', r')) = APPENDING (ok-1, f', x::r')
    | exec state = state

  fun invalidate (REVERSING (ok, f, f', r, r')) = REVERSING (ok-1, f, f', r, r')
    | invalidate (APPENDING (0, f', x::r')) = DONE r'
    | invalidate (APPENDING (ok, f', r')) = APPENDING (ok-1, f', r')
    | invalidate state = state

  fun exec2 (lenf, f, state, lenr, r) =
    case exec (exec state) of
      DONE newf => (lenf, newf, IDLE, lenr, r)
    | newstate => (lenf, f, newstate, lenr, r)

  fun check (q as (lenf, f, state, lenr, r)) =
    if lenr <= lenf then exec2 q
    else let val newstate = REVERSING (0, f, [], r, [])
         in exec2 (lenf+lenr, f, newstate, 0, []) end

  val empty = (0, [], IDLE, 0, [])
  fun isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

  fun snoc ((lenf, f, state, lenr, r), x) = check (lenf, f, state, lenr+1, x::r)

  fun head (lenf, [], state, lenr, r) = raise Empty
    | head (lenf, x::f, state, lenr, r) = x

  fun tail (lenf, [], state, lenr, r) = raise Empty
    | tail (lenf, x::f, state, lenr, r) = check (lenf-1, f, invalidate state, lenr, r)
end

fun flip f (x, y) = f (y ,x)


val q1 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1];
val q2 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2];
val q3 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3];
val q4 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3,4];
val q5 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3,4,5];
