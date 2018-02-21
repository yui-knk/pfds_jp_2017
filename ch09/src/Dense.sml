structure Dense =
struct
  datatype Digit = Zero | One
  type Nat = Digit list

  fun inc [] = [One]
    | inc (Zero :: ds) = One :: ds
    | inc (One :: ds) = Zero :: inc ds

  fun dec [One] = []
    | dec (One :: ds) = Zero :: ds
    | dec (Zero :: ds) = One :: dec ds

  fun add (ds, []) = ds
    | add ([], ds) = ds
    | add (d :: ds1, Zero :: ds2) = d :: add (ds1, ds2)
    | add (Zero :: ds1, d :: ds2) = d :: add (ds1, ds2)
    | add (One :: ds1, One :: ds2) = Zero :: inc (add (ds1, ds2))
end

val one = Dense.inc ([])
val two = Dense.add (one, one)
val three = Dense.add (one, two)
val four = Dense.add (two, two)

