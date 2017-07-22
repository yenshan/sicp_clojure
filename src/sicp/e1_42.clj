(ns sicp.e1_42)

(defn compose [f g]
  (fn [x] (f (g x))))

(defn square [x] (* x x))

(defn e1_42-test []
  (println "((compose square inc) 6) = "
           ((compose square inc) 6)))
