(ns sicp.scale-tree)

;;
;; SICP p112 Mapping over trees
;;

;;
;; implement scale-tree normally
;;
(defn scale-tree [tree factor]
  (cond (and (coll? tree) (empty? tree)) nil
        (not (coll? tree)) (* tree factor)
        :else (cons (scale-tree (first tree) factor)
                    (scale-tree (rest tree) factor))))

;;
;; implement scale-tree by using map
;;
(defn scale-tree_map [tree factor]
  (map (fn [sub-tree]
         (if (coll? sub-tree)
           (scale-tree_map sub-tree factor)
           (* sub-tree factor)))
       tree))

(def ttree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree ttree 10)
(scale-tree_map ttree 10)


