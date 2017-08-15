(ns sicp.e2_44)

(defn beside [left right])
(defn below [bottom top])

(defn up-split [painter n]
  (if (zero? 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))
