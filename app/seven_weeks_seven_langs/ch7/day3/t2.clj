;; 使用引用在内存中创建一组账户的向量，实现用于修改账户余额的借贷函数debit和credit。
;; https://charlieharvey.org.uk/page/seven_languages_clojure_three

; I’m using an hash keyed on ac number to hold my accounts for convenience
(def accounts (ref {} )) 

; makes a new account (effectively the same as updating an existing one)
(defn new-account [newac]
  (dosync (alter accounts conj newac))) 

; retrieves an account from accounts ref 
(defn get-account [acnum]
  (@accounts acnum))

; retrieves the balance of an account
(defn account-balance [ac] (ac :balance))

; retrieves the name on an account
(defn account-name [ac] (ac :name))

; sets the balance on an account given an account number and a new balance
(defn set-balance [acnum balance]
  (new-account {acnum {:name (account-name(get-account acnum)), :balance balance}}))

; credit an account given the account number and an amount by which to credit it
(defn credit-account [acnum amt]
  (set-balance acnum (+ (account-balance (get-account acnum)) amt )))

; debit an account given the account number and an amount by which to credit it
(defn debit-account [acnum amt]
  (set-balance acnum (- (account-balance (get-account acnum)) amt )))

(println (new-account {:12345 {:name "John Smith", :balance "20"}}))
;; {:12345 {:name John Smith, :balance 20}}
(println (get-account :12345))
;; {:name John Smith, :balance 20}
(println (account-balance (get-account :12345)))
;; 20
(println (account-name (get-account :12345)))
;; John Smith
(set-balance :12345 50)
(println (get-account :12345))
;; {:name John Smith, :balance 50}

(credit-account :12345 10)
(println (get-account :12345))
;; {:name John Smith, :balance 60}

(debit-account :12345 5)
(println (get-account :12345))
;; {:name John Smith, :balance 55}
