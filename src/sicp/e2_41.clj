(ns sicp.e2_41
  (:require [sicp.nested-mapping :refer [flatmap]]))

(defn make-n-tuples
  "make all of ordered n-tuples from a collection of integers."
  [n coll]
  (cond (= n 1) (map #(list %) coll)
        (empty? coll) nil 
        :else (flatmap (fn [x]
                         (map (fn [a] (cons x a))
                              (make-n-tuples (dec n) (remove #(= x %) coll))))
                       coll)))

(make-n-tuples 2 '(1 2))
;-> ((1 2) (2 1))

(defn find-ordered-triples-sum [n s]
  (filter (fn [[a b c]] (= (+ a b c) s))
          (make-n-tuples 3 (range 1 (inc n)))))


(find-ordered-triples-sum 6 6)
