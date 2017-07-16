;;
;; Pascal Triangle
;;
(ns sicp.e1-12a
  (:gen-class))


(defn my-partition
   [[a b :as col]]
   (when (and a b)
     (cons [a b] (my-partition (rest col))))) 


(defn next-row
  [row]
  (letfn [(add-1-both-ends [coll] (concat [1] coll [1]))]
    (->> (my-partition row)
         (map (fn [[a b]] (+ a b)))
         add-1-both-ends
         vec)))

(defn pascal-triangle
  ([] (pascal-triangle [1]))
  ([col] (lazy-seq (cons col (pascal-triangle (next-row col))))))

