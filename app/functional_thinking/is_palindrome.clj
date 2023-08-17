;; clj -M is_palindrome.clj

(defn palindrome? [s]
    (let [sl (.toLowerCase s)]
    (= sl (apply str (reverse sl)))))

(defn find-palindromes [s]
    (filter palindrome? (clojure.string/split s #" ")))

;; (anna)
(println (find-palindromes "The quick brown fox jumped over anna the dog"))
;; (Bob Harrah Otto)
(println (find-palindromes "Bob went to Harrah and gambled with Otto and Steve"))
;; 缓求值序列，只从序列中取出 1 个元素
;; (Bob)
(println (take 1 (find-palindromes "Bob went to Harrah and gambled with Otto and Steve")))
