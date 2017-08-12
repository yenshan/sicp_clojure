(ns sicp.e2_36)

(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))

(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    nil
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

(def tseqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))

(accumulate-n + 0 tseqs)
