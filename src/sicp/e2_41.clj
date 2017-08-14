(ns sicp.e2_41
  (:require [sicp.nested-mapping :refer [flatmap]]))

;;
;; implement exercise 2.41, 
;;   At first, create general purpose function for making n-tuple, then use it.
;;
(defn make-n-tuples
  "make all of ordered n-tuples from a collection of integers."
  [n coll]
  (cond (= n 1) (map #(list %) coll)
        (empty? coll) nil 
        :else (flatmap
                (fn [x]
                  (map (fn [a] (cons x a))
                       (make-n-tuples (dec n) (remove #(= x %) coll))))
                coll)))

(make-n-tuples 2 '(1 2))
;-> ((1 2) (2 1))

(defn find-ordered-triples-sum [n s]
  (filter (fn [[a b c]] (= (+ a b c) s))
          (make-n-tuples 3 (range 1 (inc n)))))


(find-ordered-triples-sum 6 6)

;;
;; implement make-n-tuples by using for-macro
;;
(defn make-n-tuples2
  [n coll]
  (cond (= n 1) (map #(list %) coll)
        (empty? coll) nil 
        :else (for [x coll
                    a (make-n-tuples2 (dec n) (remove #(= x %) coll))]
                (cons x a))))

(make-n-tuples2 2 '(1 2))

;;
;; --------------------------------------------------------------------
;;

;;
;; implement exercise 2.41 function simply by using three map loops and filter
;;
(defn find-ordered-triples-sum2 [n s]
  (filter (fn [[a b c]] 
            (and (not= a b) (not= b c) (not= a c)
                 (= s (+ a b c))))
          (flatmap 
            (fn [a] (flatmap 
                      (fn [b] (map 
                                (fn [c] (list a b c))
                                (range 1 (inc n))))
                      (range 1 (inc n))))
            (range 1 (inc n)))))

(find-ordered-triples-sum2 6 6)


;;
;; impelent exercise 2.41 function by using for-macro with when
;;
(defn clj-find-ordered-triples-sum [n s]
  (for [a (range 1 (inc n))
        b (range 1 (inc n))
        c (range 1 (inc n))
        :when (and (not= a b) (not= b c) (not= a c))
        :when (= s (+ a b c))]
    (list a b c)))

(clj-find-ordered-triples-sum 6 6)
