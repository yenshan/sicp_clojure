(ns sicp.e2_62
  (:require [clojure.test :refer :all])
  (:require [sicp.set-as-ordered-list :refer :all]))

(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set2 set1))

(deftest test-e2-62
  (testing "test"
    (is (= '(1 2 3 4 5 6) (union-set '(1 2 3 4) '(3 4 5 6))))))
