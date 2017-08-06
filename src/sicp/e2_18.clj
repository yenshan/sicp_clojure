(ns sicp.e2_18)

(defn -reverse
  ([lst] (-reverse lst '()))
  ([lst res]
   (if (empty? lst)
     res
     (recur (rest lst) (cons (first lst) res)))))


(-reverse (list 1 2 3 4))
