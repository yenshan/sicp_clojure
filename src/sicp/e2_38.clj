(ns sicp.e2_38
  (:require [sicp.sec2-2-3 :refer [accumulate]]))

(defn fold-right [op initial seqc]
  (accumulate op initial seqc))

(defn fold-left [op initial seqc]
  (letfn [(iter [result rst]
            (if (empty? rst)
              result
              (iter (op result (first rst))
                        (rest rst))))]
    (iter initial seqc)))

(fold-right / 1.0 (list 1 2 3))
(fold-left / 1.0 (list 1 2 3))

(fold-right list nil (list 1 2 3))
(fold-left list nil (list 1 2 3))
