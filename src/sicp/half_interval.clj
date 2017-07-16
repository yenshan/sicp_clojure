(ns sicp.half-interval
  (gen-class))

(defn average [a b] (/ (+ a b) 2))

(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))

(defn close-enough? [x y]
  (< (Math/abs (- x y)) 0.001))


(defn search [f neg-point pos-point]
  (let [midpoint (average neg-point pos-point)]
    (if (close-enough? neg-point pos-point)
      midpoint
      (let [test-value (f midpoint)]
        (cond (positive? test-value) (recur f neg-point midpoint)
              (negative? test-value) (recur f midpoint pos-point)
              :else midpoint)))))

(defn half-interval-method [f a b]
  (let [a-value (f a)
        b-value (f b)]
    (cond (and (negative? a-value) (positive? b-value))
          (search f a b)
          (and (negative? b-value) (positive? a-value))
          (search f b a)
          :else (println "Value are not of oppsite sign" a b ))))

(defn -main [& args]
  (println "root of sin between 2 and 4 is" 
           (half-interval-method #(Math/sin %) 2.0 4.0))
  (println "root of x^3 - 2x - 3 = 0 between 1 and 2 is"
           (half-interval-method #(- (* % % %) (* 2 %) 3) 1.0 2.0)))
