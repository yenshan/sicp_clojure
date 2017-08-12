(ns sicp.e2_30)

(defn square [x] (* x x))

;;
;; definition of square-tree without using map
;;
(defn square-tree [tree]
  (cond (not (coll? tree)) (square tree)
        (empty? tree) nil
        :else (cons (square-tree (first tree))
                    (square-tree (rest tree)))))

;;
;; definition of square-tree by using map
;;
(defn square-tree_map [tree]
  (map (fn [subtree]
         (if (coll? subtree) 
           (square-tree_map subtree)
           (square subtree)))
       tree))

(def ttree (list 1 
                 (list 2 (list 3 4) 5)
                 (list 6 7)))

(defn e2_30-test []
  (println "original tree:" ttree)
  (println "square tree 1:" (square-tree ttree))
  (println "square tree 2:" (square-tree_map ttree)))

(e2_30-test)
