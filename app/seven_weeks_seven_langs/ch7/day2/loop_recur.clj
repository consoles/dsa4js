(defn size[v]
    (if (empty? v)
        0
        (inc (size (rest v)))
    )
)

;;  clj loop_recur.clj
(println (size [1 2 3]))
;; 3

;; (loop [x x-init-value, y y-init-value] (do-sth x y))
;; 。函数式语言通过使用尾递归优化技术规避递归算法导致的内存耗尽的问题。受到JVM限制，Clojure并不支持隐式的尾递归优化。你必须显式地使用loop和recur进行递归，可以把loop看作是let语句
;; 初始时，对于给定向量，loop会将向量中偶数位变量绑定到奇数位的值上。
(defn size2 [v]
    (loop [l v, c 0]
    (if (empty? l)
        c
        (recur (rest l) (inc c)))))
(println (size2 [1 2 3]))

;; 在size的第二个版本中，使用了经过尾递归优化的loop和recur。
;; 由于并不真正返回结果值，因此我们在变量中保存结果，这个变量称为累加器。在这个例子里，c用于计数。
;; 这个版本会像经过尾递归优化一样工作，但是我们却为此新增了几行看起来非常糟糕的代码。
;; 有时，JVM就像一把双刃剑。如果你需要交互，那就必须把问题处理掉。
;; 不过由于这个函数已经加入到集合的基础API中，你不会经常需要用到recur。
;; 此外，Clojure也提供了一些优秀的递归替代技术，包括后面会讲到的延迟序列
