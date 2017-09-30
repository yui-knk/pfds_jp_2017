## merge

merge ts1 ts2とし、ts1の長さをLen(ts1)、ts2の長さをLen(ts2)とする。

linkがないときの償却コストは、ts1 ts2から順番に1つずつ要素を取得したときが
最も大きく、そのときは

log(Len(ts1)) + log(Len(ts2))

程度。なおこのときlinkが走っていないため、ヒープの木の数は変化しない。

次にlinkが発生するときの償却コストを考える。
k回linkが発生したとすると、

log(Len(ts1)) + log(Len(ts2)) 程度の計算に、
linkのコスト k が追加される。ポテンシャルは、 -k 変化するので、
結局、償却コストは log(Len(ts1)) + log(Len(ts2)) 程度。


## deleteMin

removeMinTreeのコストは木の個数 t と同じなので、"log n"。
ヒープの要素数が n のとき、もっとも大きい木を `rev ts1` したとしても "-1 + log(n+1)"。
mergeは上記の通り、"log n"に収まる。

よってたかだか "3 * log n"程度。
