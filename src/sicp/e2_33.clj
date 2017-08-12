(ns sicp.e2_33)

(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))

;;
;; definition of map by using accumulate
;;
(defn -map [p seqc]
  (accumulate (fn [x y] (cons (p x) y))
              nil seqc))

;; using Clojure's reduce
(defn -map' [p seqc]
  (reduce (fn [x y] (concat x (list (p y))))
          nil seqc))

(-map inc (list 1 2 3 4))
(-map' inc (list 1 2 3 4))

;;
;; definition of append by using accumulate
;;
(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

;; using Clojure's reduce
(defn append' [seq1 seq2]
  (reduce #(concat %1 (list %2)) seq1 seq2))
  
(append (list 1 2 3) (list 3 4 5))
(append' (list 1 2 3) (list 3 4 5))

;;
;; definition of length by using accumulate
;;
(defn length [seqc]
  (accumulate (fn [x y] (inc y)) 0 seqc))

;; using Clojure's reduce
(defn length' [seqc]
  (reduce (fn [x y] (inc x)) 0 seqc))

(length (list 1 2 3 4 5))
(length' (list 1 2 3 4 5 6))
