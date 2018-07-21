datatype 'a Seq = NIL | CONS of 'a * ('a * 'a) Seq

val three = CONS(0, CONS((1, 2), NIL))

(*Error: operator and operand don't agree [circularity]
  operator domain: 'Z Seq
  operand:         ('Z * 'Z) Seq
  in expression:
    size ps
*)
(*fun size NIL = 0
  | size (CONS (x, ps)) = 1 + 2 * size(ps)
*)