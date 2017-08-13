(ns sicp.nested-mapping
  (:require [sicp.sec2-2-3 :refer [accumulate enumerate-interval]])
  (:require [sicp.find-divisor :refer [prime?]]))

;(defn append [coll x]
;  (if (nil? x)
;    coll
;    (concat coll x)))

;; definition of exercise 2.33
(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

;;
;; generate pair of (i, j) when 1 <= j < i <= n
;; by using nested map
;;
(defn test1 [n]
  (map (fn [i]
         (map (fn [j] (list i j))
              (enumerate-interval 1 (- i 1))))
       (enumerate-interval 1 n)))
(test1 3)
;-> (() (2 1) ((3 1) (3 2)))

(defn test2 [n]
  (accumulate append 
              nil
              (map (fn [i]
                     (map (fn [j] (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(test2 3)
;-> ((2 1) (3 1) (3 2))

(defn test3 [n]
  (accumulate append
              nil 
              (map (fn [i]
                     (map (fn [j] (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
(test3 6)

;;
;;
;;
(defn flatmap [proc seqc]
  (accumulate append nil (map proc seqc)))

(defn prime-sum? [pair]
  (prime? (+ (first pair) (second pair))))

(prime-sum? '(1 2))
(prime-sum? '(1 3))

;;
;; definition make-pair-sum, prime-sum-pairs
;;
(defn make-pair-sum [pair]
  (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (fn [i]
                          (map (fn [j] (list i j))
                               (enumerate-interval 1 (- i 1))))
                          (enumerate-interval 1 n)))))

(test2 6)
(prime-sum-pairs 6)

;;
;; permutations
;;

(defn -remove [item seqc]
  (filter (fn [x] (not (= x item))) seqc))

(defn permutations [s]
  (if (empty? s)
    (list nil)
    (flatmap (fn [x]
               (map (fn [p] (cons x p))
                    (permutations (-remove x s))))
             s)))

(permutations '(1 2 3))

