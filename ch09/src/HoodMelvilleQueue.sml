use "SkewBinaryNumberRandomAccessList.sml";

signature QUEUE = 
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

signature QUEUEEXTENDED =
sig
  include QUEUE

  val lookup : int * 'a Queue -> 'a
  (*val update : int * 'a * 'a Queue -> 'a Queue*)
end

structure HoodMelvilleQueue : QUEUEEXTENDED =
let
  structure S = SkewBinaryNumberRandomAccessList
in
  struct
    datatype 'a RotationState =
        IDLE
      | REVERSING of int * 'a S.RList * 'a S.RList * 'a S.RList * 'a S.RList
      | APPENDING of int * 'a S.RList * 'a S.RList
      | DONE of 'a S.RList

    type 'a Queue = int * 'a S.RList * 'a RotationState * int * 'a S.RList

    (* 処理内容によって、`f + 1 = r`を常に担保している *)
    fun exec (REVERSING (ok, [], f', rs, r')) = APPENDING (ok, f', S.cons (S.head rs, r'))
      | exec (REVERSING (ok, fs, f', rs, r')) =
          REVERSING (ok+1, S.tail fs, S.cons (S.head fs, f'), S.tail rs, S.cons (S.head rs, r'))
      | exec (APPENDING (0, f', r')) = DONE r'
      | exec (state as APPENDING (ok, [], r')) = state
      | exec (APPENDING (ok, fs, r')) = APPENDING (ok-1, S.tail fs, S.cons (S.head fs, r'))
      | exec state = state

    fun invalidate (REVERSING (ok, f, f', r, r')) = REVERSING (ok-1, f, f', r, r')
      | invalidate (state as APPENDING (0, f', [])) = state
      | invalidate (APPENDING (0, f', rs)) = DONE (S.tail rs)
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

    fun snoc ((lenf, f, state, lenr, r), x) = check (lenf, f, state, lenr+1, S.cons (x, r))

    fun head (lenf, [], state, lenr, r) = raise Empty
      | head (lenf, ts, state, lenr, r) = S.head ts

    fun tail (lenf, [], state, lenr, r) = raise Empty
      | tail (lenf, ts, state, lenr, r) = check (lenf-1, S.tail ts, invalidate state, lenr, r)

    fun count ([], s) = s
      | count ((w, t) :: ts, s) = count (ts, s + w)

    fun lookup (i, (lenf, [], state, lenr, rs))= raise Subscript
      | lookup (i, (lenf, ts, state, lenr, rs)) =
          if i < count (ts, 0) then S.lookup (i, ts)
          else S.lookup (i - count (ts, 0), rs)
  end
end

fun flip f (x, y) = f (y, x)

val q1 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1];
val q2 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2];
val q3 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3];
val q4 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3,4];
val q5 = foldl (flip HoodMelvilleQueue.snoc) HoodMelvilleQueue.empty [1,2,3,4,5];
