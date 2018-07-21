signature ORDERED = 
sig
  type T

  val eq  : T * T -> bool
  val lt  : T * T -> bool
  val leq : T * T -> bool
end

structure IntOrdered : ORDERED =
struct
  type T = int

  fun eq  (a, b) = (a = b)
  fun lt  (a, b) = (a < b)
  fun leq (a, b) = (a <= b)
end
