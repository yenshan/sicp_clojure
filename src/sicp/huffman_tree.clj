(ns sicp.huffman-tree
  (:require [clojure.test :refer :all]))

(defn make-leaf [symbol weight]
  (list 'leaf symbol weight))

(defn leaf? [object]
  (= (first object) 'leaf))

(defn symbol-leaf [x] (second x))

(defn weight-leaf [x] (nth x 2))

(defn symbols [tree]
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (nth tree 2)))

(defn weight [tree]
  (if (leaf? tree)
    (weight-leaf tree)
    (nth tree 3)))

(defn make-code-tree [left right]
  (list left
        right
        (concat (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))

(defn choose-branch [bit branch]
  (cond (= bit 0) (left-branch branch)
        (= bit 1) (right-branch branch)
        :else println "back bit -- CHOOSE BRANCH" bit))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits)
              '()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (cons (symbol-leaf next-branch)
                        (decode-1 (rest bits) tree))
                  (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(defn adjoin-set [x set]
  (cond (empty? set) (list x)
        (< (weight x) (weight (first set))) (cons x set)
        :else (cons (first set)
                    (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
  "construct ordered set of leaves from a list of symbol-frequency pairs such as 
   ((A 4) (B 2) (C 1) (C 1))"
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)     ;symbol
                             (second pair))   ;frequency
                  (make-leaf-set (rest pairs))))))

;;
;; testing code
;;
(deftest test-huffman-tree
  (testing "leaf? symbol-leaf, weight-leaf"
    (is (leaf? (make-leaf "a" 1)))
    (is (= "a" (symbol-leaf (make-leaf "a" 1))))
    (is (= 1 (weight-leaf (make-leaf "a" 1))))
    )
  (testing "symbols, weight"
    (let [tree (make-code-tree (make-leaf "a" 1)
                                (make-leaf "b" 2))]
      (is (= '("a" "b") (symbols tree)))
      (is (= 3 (weight tree)))
      ))
  (testing "decode"
    (let [tree (make-code-tree (make-leaf "G" 1)
                               (make-leaf "H" 1))]
      (is (= '("G" "H") (decode '(0 1) tree)))
      (is (= '("H" "G") (decode '(1 0) tree)))
      ))  
  (testing "make-leaf-set"
    (is (= '((leaf B 1) (leaf A 2)) (make-leaf-set '((A 2) (B 1)))))
    )
  )

