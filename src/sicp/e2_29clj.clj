(ns sicp.e2_29clj)

(defrecord Mobile [left right])
(defrecord Branch [length structure])

;;
;; exercise 2.29b
;;
(defn total-weight [mobile sum])

(defn sum-weight
  ([branch] (sum-weight branch 0))
  ([branch sum]
   (if-not (zero? (:length branch))
     (+ sum (:length branch))
     (total-weight (:structure branch) sum))))

(defn total-weight
  ([mobile] (total-weight mobile 0))
  ([mobile sum]
     (+ (sum-weight (:left mobile) sum)
       (sum-weight (:right mobile) sum))))

;;
;; exercise 2.29c
;;
(defn balanced? [mobile]
  (= (sum-weight (:left mobile))
     (sum-weight (:right mobile))))


;;
;; test
;;
(def bm (->Mobile (->Branch 50
                            nil)
                  (->Branch 0 
                            (->Mobile (->Branch 20 nil)
                                      (->Branch 30 nil)))))
(defn e2_29clj-test []
  (println (total-weight bm))
  (println (balanced? bm)))

(e2_29clj-test)
