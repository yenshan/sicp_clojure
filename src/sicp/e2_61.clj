(ns sicp.e2_61
  (:require [clojure.test :refer :all]))

(defn adjoin-set [x set]
  (cond (empty? set) (cons x set)
        (= x (first set)) set
        (< x (first set)) (cons x set)
        :else (cons (first set) (adjoin-set x (rest set)))))

(defn adjoin-set-iter
  ([x set] (adjoin-set-iter x set '()))
  ([x set res]
   (cond (empty? set) (concat res (list x))
         (= x (first set)) (concat res set)
         (< x (first set)) (concat res (list x) set)
         :else (recur x (rest set) (concat res (list (first set)))))))


(deftest test-e2-61
  (testing "test"
    (is (= '(1) (adjoin-set 1 '())))
    (is (= '(1 2 3) (adjoin-set 1 '(2 3))))
    (is (= '(1 3 4) (adjoin-set 3 '(1 4))))
    (is (= '(1 2 3 4) (adjoin-set 4 '(1 2 3))))
    (is (= '(1 2 3) (adjoin-set 2 '(1 2 3))))
    (is (= '(1) (adjoin-set-iter 1 '())))
    (is (= '(1 2 3) (adjoin-set-iter 1 '(2 3))))
    (is (= '(1 3 4) (adjoin-set-iter 3 '(1 4))))
    (is (= '(1 2 3 4) (adjoin-set-iter 4 '(1 2 3))))
    (is (= '(1 2 3) (adjoin-set-iter 2 '(1 2 3))))
    ))
