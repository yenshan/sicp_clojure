(ns sicp.e2_31)

(defn square [x] (* x x))

(defn tree-map [proc tree]
  (map (fn [subtree]
         (if (coll? subtree) 
           (tree-map proc subtree)
           (proc subtree)))
       tree))

(defn square-tree [tree] (tree-map square tree))

(def ttree (list 1 
                 (list 2 (list 3 4) 5)
                 (list 6 7)))

(defn e2_31-test []
  (println "original tree:" ttree)
  (println "square tree:" (square-tree ttree)))
