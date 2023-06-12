;; 创建原子
(atom "Split at your own risk.") ; #object[clojure.lang.Atom 0x330eee55 {:status :ready, :val "Split at your own risk."}]
;; 绑定原子
(def danger (atom "Split at your own risk."))
(println danger) ; #object[clojure.lang.Atom 0x39dcf4b0 {:status :ready, :val Split at your own risk.}]
(println @danger) ; Split at your own risk.

;; 用 reset! 重新绑定 danger 
(reset! danger "Split with impunity")
(println danger) ; #object[clojure.lang.Atom 0x39dcf4b0 {:status :ready, :val Split with impunity}]

;; reset! 替换了整个原子，不过首选方法是提供一个函数来变换原子，如果要修改一个很大的向量，可以使用 swap! 原地修改原子
(def top-sellers (atom []))
(swap! top-sellers conj {:title "Seven Languages in Seven Weeks", :author "Tate"})
(println @top-sellers) ; [{:title Seven Languages in Seven Weeks, :author Tate}]
(swap! top-sellers conj {:title "Programming Clojure", :author "David"})
;; 和引用一样，一次创建好值，然后用swap!来修改
(println @top-sellers) ; [{:title Seven Languages in Seven Weeks, :author Tate} {:title Programming Clojure, :author David}]

;; 构建原子缓存
(defn create
    []
    (atom {}))
;; 根据缓存获取的键获取元素
(defn get
    [cache key]
    (@cache key))
;; 向缓存中放入元素
(defn put
    ([cache value-map]
        (swap! cache merge value-map))
    ([cache key value]
        (swap! cache assoc key value)))   

(def ac (create))
(put ac :quote "I'm your father, Luke.")
(println (str "Cached item: " (get ac :quote))) ; Cached item: I'm your father, Luke.
