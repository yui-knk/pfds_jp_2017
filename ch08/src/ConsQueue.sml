signature QUEUE = 
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  val snoc    : 'a Queue * 'a -> 'a Queue
  val head    : 'a Queue -> 'a
  val tail    : 'a Queue -> 'a Queue
end

signature CONSQUEUE = 
sig
  include QUEUE

  val cons    : 'a * 'a Queue -> 'a Queue
end

functor ConsQueue (Q : QUEUE) : CONSQUEUE =
struct
  type 'a Queue = 'a list * 'a Q.Queue

  val empty = ([], Q.empty)
  fun isEmpty (([], q)) = Q.isEmpty (q)
    | isEmpty ((x :: l, q)) = false

  fun snoc ((l, q) , x) = (l, Q.snoc (q, x))
  fun head (([], q)) = Q.head (q)
    | head ((x :: l, q)) = x
  fun tail (([], q)) = ([], Q.tail (q))
    | tail ((x :: l, q)) = (l, q)

  fun cons (a, (l, q)) = (a :: l, q)
end

structure SimpelQueue : QUEUE =
struct
  type 'a Queue = 'a list * 'a list

  val empty = ([], [])
  fun isEmpty (r, f) = (null r) andalso (null f)

  fun check ((r, f)) =
    if null r then (r @ rev (f) , []) else (r, f)

  fun snoc ((r, f), x) = check(r, x :: f)

  fun head ([], []) = raise Empty
    | head (x :: r, f) = x

  fun tail ([], []) = raise Empty
    | tail (x :: r, f) = (r, f)
end

structure SimpleConsQueue = ConsQueue (SimpelQueue)

val a = SimpleConsQueue.cons (1, SimpleConsQueue.empty)

