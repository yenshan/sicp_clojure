(ns sicp.set-as-unordered-list
  (:require [clojure.test :refer :all]))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (first set)) true
        :else (element-of-set? x (rest set))))

(defn adjoin-set [x set]
  (if (element-of-set? x set)
    set
    (cons x set)))

(defn intersection-set [set1 set2]
  (cond (or (empty? set1) (empty? set2)) '()
        (element-of-set? (first set1) set2)
          (cons (first set1)
                (intersection-set (rest set1) set2))
        :else (intersection-set (rest set1) set2)))


(deftest test-set
  (testing "test set as unordered list"
    (is (element-of-set? 'x '(x y)))
    (is (= '(b c) (intersection-set '(a b c) '(b c d))))
    ))

