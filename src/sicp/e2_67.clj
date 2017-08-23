(ns sicp.e2_67
  (:require [clojure.test :refer :all])
  (:require [sicp.huffman-tree :refer :all]))

(def sample-tree 
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(def sameple-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(deftest test-e2_67
  (testing "decode"
    (is (= '(A D A B B C A) (decode sameple-message sample-tree)))
    ))
