(ns sicp.e1_36)

(defn average [x y] (/ (+ x y) 2))

(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) tolerance))
          (try-f [guess]
            (println "guess: " guess)
            (let [next-num (f guess)]
              (if (close-enough? guess next-num)
                next-num
                (try-f next-num))))]
    (try-f first-guess))) 

(defn run-non-avd [& args]
  "without average damping"
  (println "fixed point of x^x = 1000 is "
           (fixed-point #(/ (Math/log 1000) (Math/log %))
                        10.0)))

(defn run-avd [& args]
  "with average damping"
  (println "fixed point of x^x = 1000 is "
           (fixed-point (fn [x] (average x 
                                         (/ (Math/log 1000) (Math/log x))))
                        10.0)))
