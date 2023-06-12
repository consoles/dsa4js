(def finer-things (future (Thread/sleep 5000) "take time"))
;; 5 秒后打印 take time
(println @finer-things)
