(ns sicp.e2_34)


(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))

(defn horner-eval [x coefficient-seq]
  (accumulate (fn [this-ceff higher-terms]
                (+ (* higher-terms x) this-ceff))
              0
              coefficient-seq))

(defn e2_34-test []
  " calculate 1 + 3*x + 5*x^3 + x^5, when x = 2"
  (= 79 
     (horner-eval 2 (list 1 3 0 5 0 1))))
