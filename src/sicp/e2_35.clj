(ns sicp.e2_35)

(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))

(defn enumerate-tree [tree]
  (cond (and (coll? tree) (empty? tree)) nil
        (not (coll? tree)) (list tree)
        :else (concat (enumerate-tree (first tree))
                      (enumerate-tree (rest tree)))))

(defn count-leaves [t]
  (accumulate + 0 (map (fn [x] 1) (enumerate-tree t))))


(def ttree (list 1 (list 2 3) (list 4 5) 6))

(count-leaves ttree)
