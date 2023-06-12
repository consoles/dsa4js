;; 编写一个类型用defrecord实现一个协议。

;; 定义一个协议，该协议包含一个 my-method 方法
(defprotocol MyProtocol
  (my-method [this]))

;; 定义一个记录类型，该类型包含一个名为 value 的字段
;; 将 MyProtocol 添加为 MyRecord 的实现
(defrecord MyRecord [value]
  MyProtocol
  (my-method [this]
    (print "My value is: " value)))

(let [record (->MyRecord 10)]
  (my-method record)) ; 输出 "My value is: 10"
