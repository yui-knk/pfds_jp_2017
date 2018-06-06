use "ORDERED.sml";

signature HEAP = 
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  val findMin   : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end

structure IntOrdered : ORDERED =
struct
  type T = int

  fun eq  (a, b) = (a = b)
  fun lt  (a, b) = (a < b)
  fun leq (a, b) = (a <= b)
end
