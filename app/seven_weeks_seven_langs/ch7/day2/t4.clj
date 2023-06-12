;; 用宏实现一个包含else条件的unless

;; 该宏接受一个表达式和一个代码块。如果表达式的值为 false，则执行代码块。否则，宏将执行一个包含在 else 分支中的代码块，该代码块打印一条消息。
(defmacro unless [expr & body]
  `(if (not ~expr)
     (do ~@body)
     (do (println "Executing else branch!")))) ; 这里加入了一个 else 条件，当条件为真时打印一条消息

(unless (= 1 2)
  (println "1 is not equal to 2"))

;; clj -M .\t4.clj
;; 1 is not equal to 2
