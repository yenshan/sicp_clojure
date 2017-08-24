(ns sicp.e2_69
  (:require [clojure.test :refer :all])
  (:require [sicp.huffman-tree :refer [adjoin-set make-leaf-set make-code-tree]]))

(defn successive-merge [coll]
  (if (= 1 (count coll))
    coll
    (let [new-elem (make-code-tree (second coll) (first coll))
          new-coll (adjoin-set new-elem (rest (rest coll)))]
      (successive-merge new-coll))))

(defn generate-huffman-tree [pairs]
  (successive-merge (make-leaf-set pairs)))



(deftest test-e2_69
  (testing "generate-huffman-tree"
    (is (= '(((leaf A 8) ((leaf B 3) (leaf C 1) (B C) 4) (A B C) 12)) 
           (generate-huffman-tree '((A 8) (B 3) (C 1)))))
    (is (= '(((leaf A 8) ((leaf B 3) ((leaf C 1) (leaf D 1) (C D) 2) (B C D) 5) (A B C D) 13))
           (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1)))))
    ))


