(ns sicp.e2_32)

(defn subset [s]
  (if (empty? s)
    (list '())
    (let [rst (subset (rest s))]
      (concat rst (map (fn [coll]
                         (cons (first s) coll))
                       rst)))))

(defn e2_32-test []
  (println (subset (list 1 2 3))))

(e2_32-test)
