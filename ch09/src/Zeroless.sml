structure Zeroless =
struct
  datatype Digit = ONE | TWO
  type Nat = Digit list

  fun inc [] = [ONE]
    | inc (ONE :: ds) = TWO :: ds
    | inc (TWO :: ds) = ONE :: inc ds

  fun dec [ONE] = []
    | dec (TWO :: ds) = ONE :: ds
    | dec (ONE :: ds) = TWO :: dec ds

  fun add (ds, []) = ds
    | add ([], ds) = ds
    | add (ONE :: ds1, ONE :: ds2) = TWO :: add (ds1, ds2)
    | add (TWO :: ds1, ONE :: ds2) = ONE :: inc (add (ds1, ds2))
    | add (ONE :: ds1, TWO :: ds2) = ONE :: inc (add (ds1, ds2))
    | add (TWO :: ds1, TWO :: ds2) = TWO :: inc (add (ds1, ds2))
end

val one = Zeroless.inc []
val two = Zeroless.inc one

val two' = Zeroless.add (one, one)
val three = Zeroless.add (one, two)
val three' = Zeroless.add (two', one)
val four = Zeroless.add (two', two)


