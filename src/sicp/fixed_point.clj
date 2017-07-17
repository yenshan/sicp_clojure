(ns sicp.fixed-point
  (gen-class))

;;
;; function which searchs fixed point of any function
;;

(def tolerance 0.00001)
(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) tolerance))
          (try-f [guess]
            (let [next-num (f guess)]
              (if (close-enough? guess next-num)
                next-num
                (try-f next-num))))]
    (try-f first-guess))) 

(defn test1 [& args]
  (println "fixed point of cos is "
           (fixed-point #(Math/cos %) 1.0))
  (println "fixed point of y = sin y + con y is"
           (fixed-point #(+ (Math/sin %) (Math/cos %)) 1.0)))

;;
;; Implementation of sqrt by using fixed-point
;;
(defn average [x y] (/ (+ x y) 2))
(defn sqrt [x]
  (fixed-point (fn [y] (average y (/ x y))) 1.0))

(defn test2 [& args]
  (println "fixed point of square root 2 is"
           (sqrt 2.0)))


