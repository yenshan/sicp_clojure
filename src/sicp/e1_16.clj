(ns sicp.e1_16
  (gen-class))

(defn expt
  [b n]  
  (if (zero? n)
    1
    (* b (expt b (dec n)))))

(defn fast-expt-iter
  ([b n] (fast-expt-iter b n 1))
  ([b n a] (cond
             (zero? n) a
             (even? n) (recur (* b b) (/ n 2) a)
             :else (recur b (dec n) (* b a)))))

(defn expt-iter
  ([b n] (expt-iter b n 1))
  ([b n a]
   (if (zero? n)
     a
     (recur b (dec n) (* b a)))))

