演習問題 5.1 (b)

操作前のリストfの長さをf、
操作前のリストrの長さをr、
操作後のリストfの長さをf'、
操作後のリストrの長さをr'

とする。

つまり、操作の前後でリストの長さは

f -> f'
r -> r'

へと変化する。


## cons / snoc

1. fからrへの移動がおきないケース
  このときは t = 1。ポテンシャルの変化はたかだか -1 or +1。

2. fからrへの移動がおきるケース
  これは ([a], []) -> ([b, a], []) -> ([b], [a]) のときのみ
  発生するので、 t = 1。ポテンシャルの変化はたかだか -1。

## head / last

どのケースも t = 1。
また操作の前後でfもrも変化しないため a = t = 1。

## tail / init

fもrも空でないケースを考える。

1. rからfへの移動がおきないケース
  このときは t = 1。ポテンシャルの変化はたかだか -1 or +1。

2. rからfへの移動がおきるケース
  rの半分の要素を reverse するコストは、rに等しいので t = r。
  ポテンシャルは、ざっくり r から 0 へ変化するので、
  ポテンシャルの変化は約 -r 。

  よって a = r - r = 0。