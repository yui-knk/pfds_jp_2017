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


structure SegmentedZeroOne3 =
struct
  datatype Digits = ZERO | ONES of int | TWO | THREES of int | FOUR
  (*
    (2|1|3|2(1|3)*(0|4))*  を維持する。
    そうすれば、0や4が連続することがなく、繰り下がりや繰り上がりが連続することがない。
    また、最初の桁は1か2か3となる。
  *)
  type Nat = Digits list

  fun ones (0, ds) = ds
    | ones (i, ONES j :: ds) = ONES (i + j) :: ds
    | ones (i, ds) = ONES i :: ds

  fun threes (0, ds) = ds
    | threes (i, THREES j :: ds) = THREES (i + j) :: ds
    | threes (i, ds) = THREES i :: ds

  (*
    最初の桁は1か2か3かなので、そのケースだけを考慮する。
  *)
  fun simpleinc []               = [ONES 1]
    | simpleinc (ONES i :: ds)   = TWO :: ones (i - 1, ds)
    | simpleinc (TWO :: ds)      = threes (1, ds)
    | simpleinc (THREES i :: ds) = FOUR :: threes (i - 1, ds)

  fun simpledec []               = []
    | simpledec [ONES 1]         = []
    | simpledec (ONES i :: ds)   = ZERO :: ones (i - 1, ds)
    | simpledec (TWO :: ds)      = ones (1, ds)
    | simpledec (THREES i :: ds) = TWO :: threes (i - 1, ds)

  (*
    simpleinc

    * 1 -> 2
    * 2 -> 3
    * 3 -> 4

    simpledec

    * 1 -> 0
    * 2 -> 1
    * 3 -> 2

    最初の桁が

    * 0 -> もともと1でdecをしたケース。もとが1なので、下から2桁目は(1|2|3)。
           よって1つ上の桁を繰り下げる。
    * 4 -> もともと3でincをしたケース。もとが1なので、下から2桁目は(1|2|3)。
           よって1つ上の桁へ繰り上げる。
    * 3 -> もともと2でincをしたケース。連続した3のあとに、(0|4)がくるケースは不変条件を
           満たさないので、調整をする。

      3330X -> 3332 dec
      3334X -> 3332 inc

      * X は不変条件より(1|2|3)

    * 1 -> もともと2でdecをしたケース。連続した1のあとに、(0|4)がくるケースは不変条件を
           満たさないので、調整をする。

      1110X -> 1112 dec
      1114X -> 1112 inc

      * X は不変条件より(1|2|3)

    * 2 -> とくにおこなうことはない。
  *)
  fun fixup ([ZERO]) = []
    | fixup (ZERO :: ds) = TWO :: simpledec ds
    | fixup (FOUR :: ds) = TWO :: simpleinc ds
    | fixup (THREES i :: ZERO :: ds) = THREES i :: TWO :: simpledec ds
    | fixup (THREES i :: FOUR :: ds) = THREES i :: TWO :: simpleinc ds
    | fixup (ONES i :: ZERO :: ds) = ONES i :: TWO :: simpledec ds
    | fixup (ONES i :: FOUR :: ds) = ONES i :: TWO :: simpleinc ds
    | fixup ds = ds

  fun inc ds = fixup (simpleinc ds)
  fun dec [] = raise Empty
    | dec ds = fixup (simpledec ds)

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

  fun decWhile_ 0 li acc = acc
    | decWhile_ i li acc =
      let
        val t = dec li
      in
        decWhile_ (i - 1) t (t :: acc)
      end

  fun decWhile i li = decWhile_ i li []

end

(*val t = foldl (fn (_, l) => SegmentedZeroOne2.inc l) [] [1,2,3,4,5,6,7,8,9]*)

(*SegmentedZeroOne2.zeroOneList 10*)
val ll = SegmentedZeroOne3.zeroOneList 10
val l1 = foldl (fn ((_, l), _) => l) [] ll
(*val l2 = SegmentedZeroOne3.decWhile 9 l1*)

