(ns sicp.e2_68
  (:require [clojure.test :refer :all])
  (:require [sicp.huffman-tree :refer :all]))

(defn contain? [sym coll]
  (not= nil (some #(= sym %) coll)))

(defn encode-symbol
  ([symbol tree] (encode-symbol symbol tree []))
  ([symbol tree bits]
   (if (leaf? tree)
     (if (contain? symbol (symbols tree)) 
       bits
       (println "not symbol in tree:" symbol))
     (cond (contain? symbol (symbols (left-branch tree))) 
            (encode-symbol symbol
                          (left-branch tree)
                          (conj bits  0))
           (contain? symbol (symbols (right-branch tree))) 
            (encode-symbol symbol
                          (right-branch tree)
                          (conj bits 1))
            :else (println "not symbol in tree:" symbol)))))

(defn encode [message tree]
  (if (empty? message)
    [] 
    (concat (encode-symbol (first message) tree)
            (encode (rest message) tree))))
             

(def sample-tree 
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(deftest test-e2_68
  (testing "ecode-symbol"
    (is (= [0] (encode-symbol 'A sample-tree)))
    (is (= [1 0] (encode-symbol 'B sample-tree)))
    (is (= [1 1 1] (encode-symbol 'C sample-tree)))
    (is (= [1 1 0] (encode-symbol 'D sample-tree))))
  (testing "encode"
    (is (= '(0 1 1 0 0 1 0 1 0 1 1 1 0) (encode '(A D A B B C A) sample-tree)))
    ))
