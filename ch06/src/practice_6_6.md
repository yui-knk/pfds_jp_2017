# (a)

## 補足

停止リストは一枚岩である。

```
$ sml ch06/src/practice_6_6.sml

- force l2;
val it = [1,2,3,4,5] : int list

- force s;
val it = CONS (2,$$) : int StreamCell
```

## 本題

empty に対して snoc を7回連続で実行したのちに、 tail を実行することを考える。

```
(* lazy_queue.sml *)
val a = Queue ([1],1,$$,0,[]) : int Queue
val b = Queue ([1],3,$$,0,[]) : int Queue
val c = Queue ([1,2,3],7,$$,0,[]) : int Queue
val d = Queue ([2,3],6,$$,0,[]) : int Queue

(* practice_6_6.sml *)
val a = Queue ([1],1,$$,0,[]) : int Queue
val b = Queue ([1],3,$$,0,[]) : int Queue
val c = Queue ([1],7,$$,0,[]) : int Queue
val d = Queue ([2,3,4,5,6,7],6,$$,0,[]) : int Queue
```

後者は tail 時に長さ 6のlazy listが進行しており、0(1)償却時間に収まっていない。
