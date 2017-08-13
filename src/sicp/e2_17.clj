(ns sicp.e2_17)

(defn last-pair [lst]
  (if (empty? (rest lst))
    lst
    (last-pair (rest lst))))

(last-pair (list 23 72 149 34))
