(ns sicp.e1_45
  (:require [sicp.average-damp :refer [average-damp]])
  (:require [sicp.fixed-point :refer [fixed-point]])
  (:require [sicp.e1_43 :refer [repeated]]))

(defn cube [x] (* x x x))

(defn exp
  ([a n] (exp a n 1))
  ([a n res]
   (if (zero? n)
     res
     (recur a (dec n) (* a res)))))

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1.0))

(defn check-nth-root [x n-root avd-n]
  (fixed-point ((repeated average-damp avd-n) 
                (fn [y] (/ x (exp y (dec n-root)))))
               1.0))
;;
;; nth root    average damp nth
;;    2            1
;;    3            1
;;    4            2
;;    :            :
;;    7            2
;;    8            3
;;    :            :
;;
;; log2 n times of average damps are required for find n th roots.

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn repeat-average-damp [n-root]
  (repeated average-damp (int (log2 n-root))))

(defn nth-root [x n-root]
  (fixed-point ((repeat-average-damp n-root) 
                (fn [y] (/ x (exp y (dec n-root)))))
               1.0))

