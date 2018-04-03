structure SegmentedZeroOne =
struct
  datatype DigitBlock = ZEROS of int | ONES of int
  type Nat = DigitBlock list

  fun zeros (i, []) = []
    | zeros (0, blks) = blks
    | zeros (i, ZEROS j :: blks) = ZEROS (i + j) :: blks
    | zeros (i, blks) = ZEROS i :: blks

  fun ones (0, blks) = blks
    | ones (i, ONES j :: blks) = ONES (i + j) :: blks
    | ones (i, blks) = ONES i :: blks

  fun inc ([]) = [ONES 1]
    | inc (ZEROS i :: blks) = ones (1, zeros (i - 1, blks))
    | inc (ONES i :: blks)  = ZEROS i :: inc blks

  fun dec (ONES i :: blks)  = zeros (1, ones (i - 1, blks))
    | dec (ZEROS i :: blks) = ONES i :: dec blks
end

structure SegmentedZeroOne2 =
struct
  datatype Digits = ZERO | ONES of int | TWO
  (*
    (0|1|01*2)* を維持する。
    そうすれば、2が連続することはなく、繰り上がりが連続することがない。
    また、最初の桁は0か1となる。
  *)
  type Nat = Digits list

  fun ones (0, ds) = ds
    | ones (i, ONES j :: ds) = ONES (i + j) :: ds
    | ones (i, ds) = ONES i :: ds

  fun simpleinc []             = [ONES 1]
    | simpleinc (ZERO :: ds)   = ones (1, ds)
    | simpleinc (ONES i :: ds) = TWO :: ones (i - 1, ds)

  (*
    初手がTWO -> 最初の桁は0か1にする
    初手がONE -> はじめて1でなくなった次の桁がTWOであることは回避する

    冗長二進数ではk桁目のTWOはk+1桁目のONEに相当する
  *)
  fun fixup (TWO :: ds)           = ZERO :: simpleinc ds
    | fixup (ONES i :: TWO :: ds) = ONES i :: ZERO :: simpleinc ds
    | fixup ds = ds

  fun inc ds = fixup (simpleinc ds)


  fun numList_ 0 acc = acc
    | numList_ i acc = numList_ (i - 1) (i :: acc)

  fun numList i = numList_ i []

  fun zeroOneList_ 0 acc = acc
    | zeroOneList_ i acc =
      let
        val zeroOne = foldl (fn (_, l) => inc l) [] (numList i)
        val t = (i, zeroOne)
      in
        zeroOneList_ (i - 1) (t :: acc)
      end

  fun zeroOneList i = zeroOneList_ i []
end

(*val t = foldl (fn (_, l) => SegmentedZeroOne2.inc l) [] [1,2,3,4,5,6,7,8,9]*)

(*SegmentedZeroOne2.zeroOneList 10*)
