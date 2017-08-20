(ns sicp.e2_64
  (:require [clojure.test :refer :all])
  (:require [sicp.set-as-binary-tree :refer [make-tree]]))

(defn partial-tree [elts n]
  (if (zero? n)
    (list '() elts)
    (let [left-size (quot (dec n) 2)
          left-result (partial-tree elts left-size)
          left-tree (first left-result)
          non-left-elts (second left-result)
          right-size (- n (inc left-size))
          this-entry (first non-left-elts)
          right-result (partial-tree (rest non-left-elts)
                                     right-size)
          right-tree (first right-result)
          remaining-elts (second right-result)]
      (list (make-tree this-entry left-tree right-tree)
              remaining-elts))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

;;
;; exercise 2.64 (a)
;; list->tree (1 3 5 7 9 11) should be 
;;             5
;;            / \
;;           1    9
;;           \   / \
;;            3 7  11
(deftest test-e2_64
  (testing "test"
    (is (= '(5 (1 () (3 () ())) (9 (7 () ()) (11 () ()))) 
           (list->tree '(1 3 5 7 9 11))))
    ))


