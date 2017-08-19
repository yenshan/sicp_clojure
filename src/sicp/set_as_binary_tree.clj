(ns sicp.set-as-binary-tree
  (:require [clojure.test :refer :all]))

(defn entry [tree] (first tree))
(defn left-branch [tree] (second tree))
(defn right-branch [tree] (nth tree 2))

(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x set]
  (cond (empty? set) false
        (= x (entry set)) true
        (< x (entry set)) (element-of-set? x (left-branch set))
        (> x (entry set)) (element-of-set? x (right-branch set))))

(defn adjoin-set [x set]
  (cond (empty? set) (make-tree x '() '())
        (= x (entry set)) set
        (< x (entry set)) (make-tree (entry set)
                                     (adjoin-set x (left-branch set))
                                     (right-branch set))
        (> x (entry set)) (make-tree (entry set)
                                     (left-branch set)
                                     (adjoin-set x (right-branch set)))))

(deftest test-set-as-binary-tree
  (testing "test"
    (is (= 2 (left-branch '(1 2 3))))
    (is (= 3 (right-branch '(1 2 3))))
    (let [tset (reduce #(adjoin-set %2 %1) '() [4 3 6 2 1])]
      (is (element-of-set? 3 tset)))
    ))
