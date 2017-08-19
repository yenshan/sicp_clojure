(ns sicp.e2_59
  (:require [clojure.test :refer :all])
  (:require [sicp.set-as-unordered-list :refer [adjoin-set]]))

(defn union-set [set1 set2]
  (if (empty? set1)
    set2
    (adjoin-set (first set1) (union-set (rest set1) set2))))

(defn union-set2 [set1 set2]
  (if (empty? set1)
    set2
    (recur (rest set1) (adjoin-set (first set1) set2))))

(defn union-set3 [set1 set2]
  (reduce #(adjoin-set %2 %1) set2 set1)) 


(deftest test-e2_59
  (testing "test union-set"
    (is (= '(a b c d f) (union-set '(a b c) '(b c d f))))
    (is (= '(a b c d f) (union-set2 '(a b c) '(b c d f))))
    (is (= '(a b c d f) (union-set3 '(a b c) '(b c d f))))
    ))
