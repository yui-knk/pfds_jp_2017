use "RANDOMACCESSLIST.sml";

(*structure RedundantBinaryRandomAccessList : RANDOMACCESSLIST =*)
structure RedundantBinaryRandomAccessList =
struct
  datatype 'a Tree = LEAF of 'a | NODE of int * 'a Tree * 'a Tree
  datatype 'a Digit =
         ZERO
       | ONES   of 'a Tree list
       | TWO    of 'a Tree * 'a Tree
       | THREES of ('a Tree * 'a Tree * 'a Tree) list
       | FOUR   of 'a Tree * 'a Tree * 'a Tree * 'a Tree

  type 'a RList = 'a Digit list

  val empty = []
  fun isEmpty ts = null ts

  fun size (LEAF x) = 1
    | size (NODE (w, t1, t2)) = w

  fun link (t1, t2) = NODE (size t1 + size t2, t1, t2)

  fun ones ([], ds) = ds
    | ones (ts, ONES ts2 :: ds) = ONES (ts@ts2) :: ds
    | ones (ts, ds) = ONES ts :: ds

  fun threes ([], ds) = ds
    | threes (ts, THREES ts2 :: ds) = THREES (ts@ts2) :: ds
    | threes (ts, ds) = THREES ts :: ds

  fun simpleInsTree (t, [])                         = [ONES [t]]
    | simpleInsTree (t, ONES (hd::tl) :: ds)        = TWO (t, hd) :: ones (tl, ds)
    | simpleInsTree (t, TWO (a, b) :: ds)           = threes ([(t, a, b)], ds)
    | simpleInsTree (t, THREES ((a,b,c)::tl) :: ds) = FOUR (t, a, b, c) :: threes (tl, ds)

  fun simpleDecTree ([ONES [t]])                 = (t, [])
    | simpleDecTree (ONES (hd::tl) :: ds)        = (hd, ZERO :: ones (tl, ds))
    | simpleDecTree (TWO (a, b) :: ds)           = (a, ones ([b], ds))
    | simpleDecTree (THREES ((a,b,c)::tl) :: ds) = (a, TWO (b, c) :: threes (tl, ds))

  fun fixup ([ZERO]) = []
    | fixup (ZERO :: ds) =
        let val (NODE (_,a,b), ds') = simpleDecTree ds in TWO (a,b) :: ds' end
    | fixup (FOUR (a,b,c,d) :: ds) = TWO (a,b) :: simpleInsTree (link (c,d), ds)
    | fixup (THREES ds1 :: ZERO :: ds) =
        let val (NODE (_,a,b), ds') = simpleDecTree ds in THREES ds1 :: TWO (a,b) :: ds' end
    | fixup (THREES ds1 :: FOUR (a,b,c,d) :: ds) = THREES ds1 :: TWO (a,b) :: simpleInsTree (link (c,d), ds)
    | fixup (ONES ds1 :: ZERO :: ds) =
        let val (NODE (_,a,b), ds') = simpleDecTree ds in ONES ds1 :: TWO (a,b) :: ds' end
    | fixup (ONES ds1 :: FOUR (a,b,c,d) :: ds) = ONES ds1 :: TWO (a,b) :: simpleInsTree (link (c,d), ds)
    | fixup ds = ds

  fun consTree (t, ds) = fixup (simpleInsTree (t, ds))
  fun unconsTree ds = let val (t, ds') = simpleDecTree ds in (t, fixup ds') end

  fun cons (x, ts) = consTree (LEAF x, ts)
  fun head ts = let val (LEAF x, _) = unconsTree ts in x end
  fun tail ts = let val (_, ts') = unconsTree ts in ts' end

end

(*val t = foldl RedundantBinaryRandomAccessList.cons RedundantBinaryRandomAccessList.empty [1,2,3,4]
val e1 = RedundantBinaryRandomAccessList.lookup (1, t)

val t4 = RedundantBinaryRandomAccessList.drop (0, t)
val t3 = RedundantBinaryRandomAccessList.drop (1, t)
val t2 = RedundantBinaryRandomAccessList.drop (2, t)
val t1 = RedundantBinaryRandomAccessList.drop (3, t)
val t0 = RedundantBinaryRandomAccessList.drop (4, t)

val c0 = RedundantBinaryRandomAccessList.create (0, "a")
val c1 = RedundantBinaryRandomAccessList.create (1, "a")
val c2 = RedundantBinaryRandomAccessList.create (2, "a")
val c3 = RedundantBinaryRandomAccessList.create (3, "a")
val c4 = RedundantBinaryRandomAccessList.create (4, "a")
val c5 = RedundantBinaryRandomAccessList.create (5, "a")
*)
