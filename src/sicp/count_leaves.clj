(ns sicp.count-leaves
  (:require [sicp.list :refer [length]]))

(defn count-leaves [x]
  (cond (not (coll? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

(count-leaves '(1 2 3))

(def lst (list 1 (list 2 3) (list 4 5)))
(length lst)
(count-leaves lst)
