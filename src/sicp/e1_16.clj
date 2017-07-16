(ns sicp.e1_16
  (gen-class))

(defn expt
  ([b n] (expt b n 1))
  ([b n a] (cond
             (zero? n) a
             (even? n) (recur (* b b) (/ n 2) a)
             :else (recur b (dec n) (* b a)))))
