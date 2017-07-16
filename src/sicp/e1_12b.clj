;;
;; Pascal Triangle
;;
(ns sicp.e1_12b
  (gen-class))

(defn next-row [row]
  (letfn [(add-one-both [coll] (concat [1] coll [1]))]
    (->> (partition 2 1 row)
         (map (fn [[a b]] (+ a b)))
         add-one-both
         vec)))

(defn pascal-triangle
  ([] (pascal-triangle [1]))
  ([col] (lazy-seq (cons col (pascal-triangle (next-row col))))))

(defn pascal-triangle-iter
  ([n] (pascal-triangle-iter n [1] []))
  ([n a b]
    (if (zero? n)
      b
      (recur (dec n) (next-row a) (conj b a)))))

