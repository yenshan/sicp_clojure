(ns sicp.e2_65
  (:require [clojure.test :refer :all])
  (:require [sicp.set-as-binary-tree :refer :all])
  (:require [sicp.e2_63 :refer [tree->list-1]])
  (:require [sicp.e2_64 :refer [list->tree]]))

(defn union-set [set1 set2]
  (reduce #(adjoin-set %2 %1) set2 (tree->list-1 set1)))

(defn intersection-set [set1 set2]
  (list->tree 
    (for [e1 (tree->list-1 set1)
          e2 (tree->list-1 set2)
          :when (= e1 e2)]
      e1)))


(deftest test-e2_65
    (testing "test"
      (let [set1 (list->tree '(1 3 5 7 13))
            set2 (list->tree '(1 3 7 9 11))]
        (is (= '(1 3 5 7 9 11 13) (tree->list-1 (union-set set1 set2))))
        (is (= '(1 3 7) (tree->list-1 (intersection-set set1 set2))))
        )))

