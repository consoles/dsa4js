;; 序列是与具体实现无关的抽象层，囊括了Clojure系统里各式的容器。序列封装了所有Clojure
;; 集合（set、映射表、向量，等等）、字符串，甚至包括文件系统结构（流、目录）。它们也为Java
;; 容器提供了公共抽象，包括Java集合、数组以及字符串。一般来说，只要支持函数first、rest
;; 和cons，就可以用序列封装起来。

;; clj -M .\sequence.clj

(println (every? number? [1 2 3 :four]))
;; false
(println (some nil? [1 2 nil]))
;; true

;; 修改序列
(defn abs [x] (if (< x 0) (- x) x))
(println (sort-by abs [-1 -4 3 2]))
;; (-1 2 3 -4)

;; 延迟计算
;; 不能真正去计算一个无穷数列，这个问题的解决方案就是延迟计算。通过这种策略，Clojure 的序列函数库只会在一个值真正用到的时候才会去计算它
(println (range 1 10 3))
;; (1 4 7)
(println (range 10))
;; (0 1 2 3 4 5 6 7 8 9)
;; 使用 range 创建出来的序列都是有限的
;; (repeat 1) 可以得到一个无限序列，其中的元素全部是 1
;; take 函数可以在无限序列中获取其有限的子集
(println (take 3 (repeat "我爱杨双")))
;; (我爱杨双 我爱杨双 我爱杨双)
;; 从向量 [:a :b :c] 的循环中取出前 5 个元素
(println (take 5 (cycle [:a :b :c])))
;; (:a :b :c :a :b)
;; 从向量 [:a :b :c] 的循环中丢弃前 2 个元素后取前 5 个元素
(println (take 5 (drop 2 (cycle [:a :b :c]))))
;; (:c :a :b :c :a)
;; 使用新的从左向右操作符 `->>` 将每个函数分别应用一个结果，这种方式和上面的写法输出结果是一致的，但是好在可读性好
(println (->> [:a :b :c] (cycle) (drop 2) (take 5)))
;; (:c :a :b :c :a)
;; 加入分隔符，把 :and 插入到所有元素的序列中间，相当于 js 中的 Array.prototype.join
(println (take 7 (interpose :and (cycle [:a :b :c]))))
;; (:a :and :b :and :c :and :a)
;; 将 2 个无穷序列 (cycle (range 2)) 和 (cycle (range 3)) 交错，接着取出前 20 个元素
(println (take 20 (interleave (cycle (range 2)) (cycle (range 3)))))
;; (0 0 1 1 0 2 1 0 0 1 1 2 0 0 1 1 0 2 1 0)
;; 偶数位是 0 1 0 1 0 1 0 1 ...
;; 奇数位是 0 1 2 0 1 2 0 1 2 ...
;; iterate 函数提供另一种创建序列的方法
(println (take 5 (iterate inc 1)))
;; (1 2 3 4 5)
(println (take 5 (iterate dec 0)))
;; (0 -1 -2 -3 -4)
;; 计算斐波那契
(defn fib-pair [[a,b]] [b (+ a b)])
(println (fib-pair [3 5]))
;; [5 8]
(println (take 5 (map first (iterate fib-pair [1 1]))))
;; (1 1 2 3 5)
;; 利用延迟序列计算第 80 个 斐波那契，性能非常出色
(println (nth (map first (iterate fib-pair [1 1])) 80))
;; 37889062373143906
;; 同理，计算阶乘,从无穷数列(iterate inc 1)中取出n个元素。然后用apply * 将它们相乘
(defn fac [n] (apply * (take n (iterate inc 1))))
(println (fac 20))
;; 2432902008176640000
