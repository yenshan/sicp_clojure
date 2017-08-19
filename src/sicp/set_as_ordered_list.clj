(ns sicp.set-as-ordered-list
  (:require [clojure.test :refer :all]))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        (< x (first set)) false
        :else (element-of-set? x (rest set))))

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

(defn intersection-set [set1 set2]
  (if (or (empty? set1) (empty? set2)) '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (= x1 x2) (cons x1
                            (intersection-set (rest set1) (rest set2)))
            (< x1 x2) (intersection-set (rest set1) set2)
            (< x2 x1) (intersection-set set1 (rest set1))))))

(deftest test-set-as-ordered-list
  (testing "test"
    (is (element-of-set? 'x '(x y z)))
    (is (= '(2 3) (intersection-set '(1 2 3) '(2 3 4))))
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
