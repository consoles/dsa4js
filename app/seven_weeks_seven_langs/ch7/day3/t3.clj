;; 接下来，我会描述一个称之为理发师问题（sleeping barber）的题目，它是由Edsger Dijkstra于1965年提出的，特点如下:

;; 1. 理发店接待顾客
;; 2. 顾客到达理发店的时间间隔随机，范围10~30毫秒
;; 3. 理发店的等待室里有三把椅子
;; 4. 理发店只有一位理发师和一把理发椅
;; 5. 当理发椅为空时，一位顾客坐上去，叫醒理发师，然后理发
;; 6. 如果所有椅子都被占用，新来的顾客就会离开
;; 7. 理发需要20毫秒
;; 8. 完成理发后，顾客会起身离开

;; 实现一个多线程程序来决定一个理发师在10秒内可以为多少位顾客理发

;; https://charlieharvey.org.uk/page/seven_languages_clojure_three

; flag to keep going when this gets set false stop threads
(def continue? (atom true))                                       

; barbers has 3 chairs 
; I use http://docs.oracle.com/javase/6/docs/api/java/util/concurrent/LinkedBlockingQueue.html#size()
; to model the waiting room. 
(def waiting-room (java.util.concurrent.LinkedBlockingQueue. 3)) 

; number of haircuts barber has done. use an agent for this. not crucial but nice
(def num-haircuts (agent 0)) 

; we will keep a list of logs for info
(def log-agent (agent ())) 

; increment my-agent by 1 
(defn ++1 [my-agent]
  (+ 1 my-agent))

; log something
(defn logmsg [my-agent msg]
  (let [now (java.time.LocalDateTime/now)]
    (println (str "[" (.format now java.time.format.DateTimeFormatter/ISO_DATE_TIME) "] " msg))))

; this is a thread to keep an eye on the rest of the threads. We don’t want the program running on indefinitely
(defn opening_hours [daylength] 
  ( (println) ; for some reason a call to my logmsg in the first line causes clojure to throw a ClassCastException 
    (send log-agent logmsg "!!!! Open for business !!!!")
    (Thread/sleep daylength)
    ;; (swap! continue? not) 是 Clojure 的一个函数调用，用于原子性地修改一个 atom 类型变量的值。在这里，continue? 是一个 atom 变量，它的初始值为 true。swap! 函数将会将 continue? 的值取反。
    (swap! continue? not)
    ; some threads may still be running when we get here. The longest wait is 30ms
    ; for a customer so wait for that plus a bit before closing the barbers
    (Thread/sleep 35)
    (send log-agent logmsg "!!!! Closing time !!!!")
    (send log-agent logmsg (str "\nBarber completed " @num-haircuts " haircuts"))
    (System/exit 0)))

; pushes a customer into the waiting room if there is a space in it, after waiting from 10..30ms
(defn customer []
  (future
    (while @continue?
     (.put waiting-room "Hairy customer" )
     ; add a customer to the queue
     (send log-agent logmsg (str "+ Customer joins queue (queue has " (.size waiting-room) " customers in)"))   
     ; customers arrive at random intervals from 10..30 ms
     (Thread/sleep (+ 20 (rand-int 11))))))

; if there is a customer in the queue, cuts their hair after waiting 20ms, increments number of haircuts 
(defn barber []
  (future
   (while @continue?
     (when-let [item (.poll waiting-room)] ; grab a customer from the queue if there is one
       (send log-agent logmsg "- Customer got haircut")    
       (Thread/sleep 20)                 ; haircuts take 20 ms 
       (send num-haircuts ++1)))))

;; dorun 是 Clojure 的一个函数，用于对一个序列进行遍历并对其中的每个元素进行求值，返回 nil，而不返回一个新的序列。dorun 的作用类似于 doall 函数，但是 doall 会返回一个新的序列，而 dorun 只会返回 nil
;; 这里的 (repeatedly 1 customer) 和 (repeatedly 1 barber) 都返回一个无限序列，但是由于我们使用了 dorun，它们只会被执行一次。在执行过程中，dorun 会对序列中的每个元素进行求值，然后返回 ni
; kicks off the loops for this barbers
(defn barbershop []
  (dorun (repeatedly 1 customer)) ; customers arrive one at a time
  (dorun (repeatedly 1 barber))   ; barber cuts customers hair 1 at a time 
  (opening_hours 10000))          ; how many haircuts in 10 seconds?

(barbershop)

;; !!!! Closing time !!!!

;; Barber completed 324 haircuts
