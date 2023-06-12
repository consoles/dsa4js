;; defrecord 用于类型，protocol 用于围绕类型来组织函数

(defprotocol Compass
    (direction [c])
    (left [c])
    (right [c]))

(def directions [:north :east :south :west]) ; 0 1 2 3

(defn turn
  [base amount]
  (rem (+ base amount) (count directions)))

;; 从:east向右转一次得到:south，从:west向右转一次得到:north，从:south向右转三次得到:east
(println (turn 1 1)) ;; 2
(println (turn 3 1)) ;; 0
(println (turn 2 3)) ;; 1

(defrecord SimpleCompass [bearing]
    Compass
    ;; direction函数在directions中查找下标为bearing的元素，例如 (directions 3)返回:west
    (direction [_] (directions bearing))
    ;; (SomeType. arg)意味着调用SimpleCompass的构造函数，并绑定arg到第一个参数上。
    ;; 在repl中输入(String. "new string")，repl会返回一个新字符串"new string"
    ;; right 向右转1次，left向右转3次
    (left [_] (SimpleCompass. (turn bearing 3)))
    (right [_] (SimpleCompass. (turn bearing 1)))
    Object
    (toString [this] (str "[" (direction this) "]"))
    )

(def c (SimpleCompass. 0))
(println c) ;; #user.SimpleCompass{:bearing 0}
(println (left c)) ;; #user.SimpleCompass{:bearing 3}
(println (right c)) ;; #user.SimpleCompass{:bearing 1}
;; 原来的罗盘并没有改变
(println c) ;; #user.SimpleCompass{:bearing 0}
(println (:bearing c)) ;; 0
