;; 一个队列实现，队列为空时阻塞并等待新的元素加入。

(ns my-queue
  (:require [clojure.core.async :refer [>!! <!! chan go-loop]]))

(defn new-queue []
  (let [q (chan)]
    (go-loop []
      (let [[val c] (<!! q)]
        (when-not (nil? val)
          (println "Queue: received" val)
          (c true)))
      (recur))
    {:put! (fn [x]
             (let [c (chan)]
               (>!! q [x c])
               (<!! c)))
     :close! (fn []
               (>!! q nil))}))

;; 在上面的代码中，我们首先使用 chan 函数创建了一个新的 channel。然后，我们使用 go-loop 宏创建了一个新线程，该线程将不断地从 channel 中读取数据，并在读取到数据时打印数据到终端中。
;; 然后，我们定义了一个包含 :put! 和 :close! 方法的 map。在 :put! 方法中，我们创建了一个新的 channel c。然后，我们将一个包含 x 和 c 的 vector 发送到 channel q 中，并使用 <!! 操作符等待 channel c 返回 true。这将阻塞 :put! 方法，直到另一个线程读取了 channel q 中的数据，并将 true 发送回 channel c 中。
;; 在 :close! 方法中，我们向 channel q 中发送一个 nil 值，以表明队列已经关闭。这将导致所有正在等待从 channel q 中读取数据的线程立即返回 nil。
