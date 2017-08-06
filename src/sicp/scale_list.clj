(ns sicp.scale-list)

(defn scale-list [items factor]
  (if (empty? items)
    nil
    (cons (* (first items) factor)
          (scale-list (rest items) factor))))

(scale-list (list 1 2 3 4 5) 10)

(defn -map [proc items]
  (if (empty? items)
    nil
    (cons (proc (first items))
          (-map proc (rest items)))))

(map #(Math/abs %) (list -10 2.5 -11.6 17))
(map #(* % %) (list 1 2 3 4))

(defn scale-list' [items factor]
  (map #(* % factor) items))

(scale-list' (list 1 2 3 4 5) 10)
