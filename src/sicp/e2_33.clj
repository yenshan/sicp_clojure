(ns sicp.e2_33)

(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))


(defn -map [p seqc]
  (accumulate (fn [x y] (cons (p x) y))
              nil seqc))

(-map inc (list 1 2 3 4))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))
  
(append (list 1 2 3) (list 3 4 5))

(defn length [seqc]
  (accumulate (fn [x y] (inc y)) 0 seqc))

(length (list 1 2 3 4 5))
