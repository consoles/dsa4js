(defstruct color :red :green :blue)

(defn red [v]
    (struct color v 0 0))

(defn green [v]
    (struct color 0 v 0))

(defn blue [v]
    (struct color 0 0 v))

;;  定义一个多重方法
;; 首先定义了 basic-colors-in 分发函数，它用一个 vector 结构返回所有非零的颜色分量
(defn basic-clolor-in [color]
    (for [[k v] color :when (not= v 0)] k))

(defmulti color-string basic-clolor-in)

;; 针对单色的几种特殊处理
(defmethod color-string [:red] [color]
    (str "Red: " (:red color)))

(defmethod color-string [:green] [color]
    (str "Green: " (:green color)))

(defmethod color-string [:blue] [color]
    (str "Blue: " (:blue color)))

(defmethod color-string :default [color]
    (str "Red: " (:red color) ", Green: " (:green color) ", Blue: " (:blue color)))


;; 如果调用时传入的参数是单色，该多重方法将执行对应的单色版本。
;; 如果我们传入复合的颜色，则会触发默认方法，由它返回所有的颜色分量值。 

;; 测试 3 种纯色
(println (color-string (struct color 5 0 0))) ;; Red: 5
(println (color-string (struct color 0 12 0))) ;; Green: 12
(println (color-string (struct color 0 0 66))) ;; Blue: 66

;; 测试复合颜色
(println (color-string (struct color 5 10 2))) ;; Red: 5, Green: 10, Blue: 2
