(ns sicp.e2_22
  (gen-class))

(defn square-list-iter
  ([items] (square-list-iter items (list)))
  ([items answer]
   (if (empty? items) (reverse answer)
    (recur (rest items)
           (cons (square (first items)) answer)))))
