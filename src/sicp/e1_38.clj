(ns sicp.e1_38
  (:require [sicp.e1_37 :refer [cont-frac]]))

;;
;; Compute e - 2 where e is the base of the natural logarithms by Euler's continued fraction expansion.
;;
(defn d-func [i]
  (cond
    (zero? i) 0
    (> i 2) (if (zero? (mod (- i 2) 3))
              (-> (- i 2) (/ 3) (* 2) (+ 2))
              1)
    :else i))

(defn test1 []
  (println "expand e - 2 is"
           (cont-frac (fn [i] 1.0) d-func 12)))
