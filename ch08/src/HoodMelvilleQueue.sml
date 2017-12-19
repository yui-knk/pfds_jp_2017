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

(* 8.2 *)
structure HoodMelvilleQueue2 : QUEUE =
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
    case exec (state) of
      DONE newf => (lenf, newf, IDLE, lenr, r)
    | newstate => (lenf, f, newstate, lenr, r)

  fun exec3 (lenf, f, state, lenr, r) =
    case exec (exec state) of
      DONE newf => (lenf, newf, IDLE, lenr, r)
    | newstate => (lenf, f, newstate, lenr, r)

  fun check (q as (lenf, f, state, lenr, r)) =
    if lenr <= lenf then exec2 q
    else let val newstate = REVERSING (0, f, [], r, [])
         in exec3 (lenf+lenr, f, newstate, 0, []) end

  val empty = (0, [], IDLE, 0, [])
  fun isEmpty (lenf, f, state, lenr, r) = (lenf = 0)

  fun snoc ((lenf, f, state, lenr, r), x) = check (lenf, f, state, lenr+1, x::r)

  fun head (lenf, [], state, lenr, r) = raise Empty
    | head (lenf, x::f, state, lenr, r) = x

  fun tail (lenf, [], state, lenr, r) = raise Empty
    | tail (lenf, x::f, state, lenr, r) = check (lenf-1, f, invalidate state, lenr, r)
end

val q2_1 = foldl (flip HoodMelvilleQueue2.snoc) HoodMelvilleQueue2.empty [1];
val q2_2 = foldl (flip HoodMelvilleQueue2.snoc) HoodMelvilleQueue2.empty [1,2];
val q2_3 = foldl (flip HoodMelvilleQueue2.snoc) HoodMelvilleQueue2.empty [1,2,3];
val q2_4 = foldl (flip HoodMelvilleQueue2.snoc) HoodMelvilleQueue2.empty [1,2,3,4];
val q2_5 = foldl (flip HoodMelvilleQueue2.snoc) HoodMelvilleQueue2.empty [1,2,3,4,5];

(* 8.3 *)
structure HoodMelvilleQueue3 : QUEUE =
struct
  datatype 'a RotationState =
      IDLE
    | REVERSING of int * 'a list * 'a list * 'a list * 'a list
    | APPENDING of int * 'a list * 'a list
    | DONE of 'a list

  (* The first field of Queue is diff, lenf - lenr. *)
  type 'a Queue = int * 'a list * 'a RotationState * 'a list

  fun exec (REVERSING (ok, x::f, f', y::r, r')) = REVERSING (ok+1, f, x::f', r, y::r')
    | exec (REVERSING (ok, [], f', [y], r')) = APPENDING (ok, f', y::r')
    | exec (APPENDING (0, f', r')) = DONE r'
    | exec (APPENDING (ok, x::f', r')) = APPENDING (ok-1, f', x::r')
    | exec state = state

  fun invalidate (REVERSING (ok, f, f', r, r')) = REVERSING (ok-1, f, f', r, r')
    | invalidate (APPENDING (0, f', x::r')) = DONE r'
    | invalidate (APPENDING (ok, f', r')) = APPENDING (ok-1, f', r')
    | invalidate state = state

  fun exec2 (diff, f, state, r) =
    case exec (exec state) of
      DONE newf => (length(newf)-length(r), newf, IDLE, r)
    | newstate => (diff, f, newstate, r)

  fun check (q as (diff, f, state, r)) =
    if 0 <= diff then exec2 q
    else let val newstate = REVERSING (0, f, [], r, [])
         in exec2 (length(f)+length(r), f, newstate, []) end

  val empty = (0, [], IDLE, [])
  fun isEmpty (diff, f, state, r) = (null f)

  fun snoc ((diff, f, state, r), x) = check (diff-1, f, state, x::r)

  fun head (diff, [], state, r) = raise Empty
    | head (diff, x::f, state, r) = x

  fun tail (diff, [], state, r) = raise Empty
    | tail (diff, x::f, state, r) = check (diff-1, f, invalidate state, r)
end

val q3_1 = foldl (flip HoodMelvilleQueue3.snoc) HoodMelvilleQueue3.empty [1];
val q3_2 = foldl (flip HoodMelvilleQueue3.snoc) HoodMelvilleQueue3.empty [1,2];
val q3_3 = foldl (flip HoodMelvilleQueue3.snoc) HoodMelvilleQueue3.empty [1,2,3];
val q3_4 = foldl (flip HoodMelvilleQueue3.snoc) HoodMelvilleQueue3.empty [1,2,3,4];
val q3_5 = foldl (flip HoodMelvilleQueue3.snoc) HoodMelvilleQueue3.empty [1,2,3,4,5];
