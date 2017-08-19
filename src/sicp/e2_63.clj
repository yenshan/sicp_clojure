(ns sicp.e2_63
  (:require [clojure.test :refer :all])
  (:require [sicp.set-as-binary-tree :refer :all]))

(defn tree->list-1 [tree]
  (if (empty? tree)
    '()
    (concat (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(defn tree->list-2 [tree]
  (letfn [(copy-to-list [tree result-list]
            (if (empty? tree)
              result-list
              (recur (left-branch tree)
                            (cons (entry tree)
                                  (copy-to-list (right-branch tree)
                                                result-list)))))]
    (copy-to-list tree '())))


(defn coll->tree [coll]
  (reduce #(adjoin-set %2 %1) '() coll))

(deftest test-e2-63
  (testing "test"
    (let [tree1 (coll->tree [7 3 9 1 5 11])
          tree2 (coll->tree [3 1 7 5 9 11])
          tree3 (coll->tree [5 3 9 1 7 11])
          ]
      (is (= '(1 3 5 7 9 11) (tree->list-1 tree1)))
      (is (= '(1 3 5 7 9 11) (tree->list-2 tree1)))
      (is (= '(1 3 5 7 9 11) (tree->list-1 tree2)))
      (is (= '(1 3 5 7 9 11) (tree->list-2 tree2)))
      (is (= '(1 3 5 7 9 11) (tree->list-1 tree3)))
      (is (= '(1 3 5 7 9 11) (tree->list-2 tree3)))
      )))



