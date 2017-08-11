(ns sicp.e2_28)

(def x (list (list 1 2) (list 3 4)))

(defn fringe [x]
   (cond (not (coll? x)) (list x)
         (empty? x) nil
         :else (concat (fringe (first x))
                       (fringe (rest x)))))

(defn e2_28-test []
  (println (fringe x))
  (println (fringe (list x x))))

(e2_28-test)
