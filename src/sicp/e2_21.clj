(ns sicp.e2_21
  (gen-class))

(defn square [x] (* x x))
(defn square-list [items]
  (if (empty? items)
    nil
    (cons (square (first items))
          (square-list (rest items)))))

(defn square-list' [items]
  (map square items))
