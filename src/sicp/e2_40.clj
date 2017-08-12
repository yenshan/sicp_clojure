(ns sicp.e2_40
  (:require [sicp.sec2-2-3 :refer [enumerate-interval]])
  (:require [sicp.nested-mapping :refer [flatmap make-pair-sum prime-sum?]]))

;;
;; SICP style of unique-pairs
;;
(defn unique-pairs [n]
  (flatmap (fn [i]
             (map (fn [j] (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(unique-pairs 6)

(defn prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)

;;
;; Clojure style of unique-pairs
;;
(defn clj-unique-pairs [n]
  (apply concat 
         (for [i (range 1 (inc n))]
           (for [j (range 1 i)]
             (list i j)))))

(clj-unique-pairs 6)

(defn clj-prime-sum-pairs [n]
  (map make-pair-sum (filter prime-sum? (clj-unique-pairs n))))

(clj-prime-sum-pairs 6)

(= (prime-sum-pairs 6) (clj-prime-sum-pairs 6))
