(ns sicp.e2_23)

(defn for-each [proc items]
  (if (empty? items)
    nil
    (do (proc (first items))
        (for-each proc (rest items)))))

(for-each (fn [x] (println x))
          (list 57 321 88))

