(defn twice [x] (* 2 x))
;; 定义一个 tribbles 的代理，初始值为 1
(def tribbles (agent 1))
;; 给代理发送一个值来修改 tribbles
(send tribbles twice)
;; 获取代理的值
(println @tribbles) ; 2

(defn slow-twice [x]
    (do
        (Thread/sleep 5000)
        (* 2 x)))
(println @tribbles) ; 2
(send tribbles slow-twice)
;; 
(println @tribbles) ; 2
(Thread/sleep 5000)
(println @tribbles) ; 4
