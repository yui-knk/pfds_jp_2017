structure SparseByWeight =
struct
  type Nat = int list

  fun carry (w, []) = [w]
    | carry (w, ws as w' :: ws') =
        if w < w' then w :: ws else carry (w * 2, ws')

  fun borrow (w, ws as w' :: ws') =
        if w = w' then ws' else w :: borrow (w * 2, ws)

  fun inc ws = carry (1, ws)
  fun dec ws = borrow (1, ws)

  fun add (ws, []) = ws
    | add ([], ws) = ws
    | add (m as w1 :: ws1, n as w2 :: ws2) =
        if w1 < w2 then w1 :: add (ws1, n)
        else if w2 < w1 then w2 :: add (m, ws2)
        else carry (2 * w1, add (ws1, ws2))
end

val one = SparseByWeight.inc []
val two = SparseByWeight.add (one, one)
val three = SparseByWeight.add (one, two)
val four = SparseByWeight.add (two, two)
