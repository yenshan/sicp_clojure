;;
;; Clojure style implementation of huffman tree
;;
(ns sicp.clj-huffman-tree
  (:require [clojure.test :refer :all]))

(defrecord Leaf [symbol weight])

(defrecord CodeTree [left right symbol weight])

(defmulti symbols (fn [e] (class e)))
(defmethod symbols Leaf [obj] (list (:symbol obj)))
(defmethod symbols CodeTree [obj] (:symbol obj))

(defn leaf? [object]
  (= Leaf (class object)))

(defn make-leaf [sym weight] (->Leaf sym weight))

(defn make-code-tree [left right]
  (->CodeTree left
              right
              (concat (symbols left) (symbols right))
              (+ (:weight left) (:weight right))))

(defn choose-branch [bit branch]
  (cond (= bit 0) (:left branch)
        (= bit 1) (:right branch)
        :else println "back bit -- CHOOSE BRANCH" bit))

(defn decode [bits tree]
  (letfn [(decode-1 [bits current-branch]
            (if (empty? bits)
              '()
              (let [next-branch (choose-branch (first bits) current-branch)]
                (if (leaf? next-branch)
                  (concat (symbols next-branch)
                        (decode-1 (rest bits) tree))
                  (decode-1 (rest bits) next-branch)))))]
    (decode-1 bits tree)))

(defn adjoin-set [x coll]
  (cond (empty? coll) (list x)
        (< (:weight x) (:weight (first coll))) (cons x coll)
        :else (cons (first coll)
                    (adjoin-set x (rest coll)))))

(defn make-leaf-set [pairs]
  "construct ordered set of leaves from a list of sym pairs such as 
   ((A 4) (B 2) (C 1) (C 1))"
  (if (empty? pairs)
    '()
    (let [pair (first pairs)]
      (adjoin-set (make-leaf (first pair)     ;sym
                             (second pair))   ;frequency
                  (make-leaf-set (rest pairs))))))

;;
;; testing code
;;
(deftest test-huffman-tree
  (testing "sym, weight"
    (let [tree (make-code-tree (make-leaf "a" 1)
                               (make-leaf "b" 2))]
      (is (= '("a" "b") (symbols tree)))
      (is (= 3 (:weight tree)))
      ))
  (testing "decode"
    (let [tree (make-code-tree (make-leaf "G" 1)
                               (make-leaf "H" 1))]
      (is (= '("G" "H") (decode '(0 1) tree)))
      (is (= '("H" "G") (decode '(1 0) tree)))
      ))  
  (testing "make-leaf-set"
    (is (= '((B 1) (A 2)) 
           (map (fn [{:keys [symbol weight]}] (list symbol weight))
                     (make-leaf-set '((A 2) (B 1))))))
    )
  )

