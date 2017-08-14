(ns sicp.test-map-for)


(defn unique-pairs [n]
  (reduce concat
          (map 
            (fn [i]
              (map (fn [j] (list i j))
                   (range 1 i)))
            (range 1 (inc n)))))

(unique-pairs 6)

(defn unique-pairs_for [n]
  (->> (for [i (range 1 (inc n))]
         (for [j (range 1 i)]
           (list i j)))
       (reduce concat)))

(= (unique-pairs 6) (unique-pairs_for 6))

(defn unique-pairs_for2 [n]
  (for [i (range 1 (inc n))]
    (for [j (range 1 i)]
      (list i j))))

(unique-pairs_for2 6)

(defn unique-pairs_for3 [n]
  (for [i (range 1 (inc n))
        j (range 1 i)]
    (list i j)))

(= (unique-pairs 6) (unique-pairs_for3 6))

