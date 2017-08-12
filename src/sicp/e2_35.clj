(ns sicp.e2_35
  (:require [sicp.sec2-2-3 :refer [accumulate enumerate-tree]]))

;;
;; count leaves by using same technique as square-tree
;;
(defn count-leaves [tree]
  (accumulate + 0
              (map (fn [subtree]
                     (if (coll? subtree)
                       (count-leaves subtree)
                       1))
                   tree)))

;;
;;
;;
(defn count-leaves-using-enumerate [t]
  (accumulate + 0 (map (fn [x] 1) (enumerate-tree t))))

;;
;;
(def ttree (list 1 (list 2 3) (list 4 5) 6))

(count-leaves ttree)
(count-leaves-using-enumerate ttree)
