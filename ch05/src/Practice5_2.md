二項ヒープの insert の償却時間が O(1) であることを示す。

二項ヒープのヒープの木、ひとつひとつに貯金が1ずつ関連づけられているとする。
insert は実際のステップを (k + 1) 行う。ただし k は link の呼び出し回数。
初期状態でヒープに t個 の木があったとしたら、 insert 後には (t - k + 1) 個の
木が存在する。貯金は t -> (t - k + 1) になるため、

a = (k + 1) + ((t - k + 1) - t)
  = 2
