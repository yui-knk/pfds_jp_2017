type 'a seq =
  | Nil
  | Cons of 'a * ('a * 'a) seq

type 'a seq1 =
  | Nil1
  | Cons1 of 'a * 'a seq1

(* Error: This expression has type ('a * 'a) seq
       but an expression was expected of type 'a seq
       The type variable 'a occurs inside 'a * 'a
 *)
let rec size = function
  | Nil -> 0
  | Cons (x, ps) -> 1 + 2 * (size ps)


let rec size1 = function
  | Nil1 -> 0
  | Cons1 (x, ps) -> 1 + size1 ps
