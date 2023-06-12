(ref "Attack of the Clones")
;; 将引用赋值给一个值
(def movie (ref "Star Wars"))
(println movie) ; #object[clojure.lang.Ref 0xf6c03cb {:status :ready, :val Star Wars}]
;; 解引用
(println (deref movie)) ; Star Wars
(println @movie) ; deref 可以简写为 @
;; 在事务中修改状态
(dosync (alter movie str ": The Empire Strikes Back"))
(println @movie) ; Star Wars: The Empire Strikes Back
(dosync (ref-set movie "Star Wars: 星球大战"))
(println @movie) ; Star Wars: 星球大战
