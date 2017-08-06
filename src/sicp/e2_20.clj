(ns sicp.e2_20)

(defn same-parity [& coll]
  (if (even? (first coll))
    (filter even? coll)
    (filter odd? coll)))

(same-parity 1 2 3 4 5)
(same-parity 2 3 4 5)
