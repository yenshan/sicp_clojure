(ns sicp.e1_46)

(defn iterative-improve [good-enough? improve]
  (letfn [(guess-loop [guess]
            (if (good-enough? guess)
              guess
              (guess-loop (improve guess))))]
    (fn [guess] (guess-loop guess))))

;;
;; rewrite sqrt in terms of iterative-improve
;;
(defn square [x] (* x x))
(defn average [x y] (/ (+ x y) 2))
(defn abs [x] (if (< x 0) (- x) x))

(defn sqrt [x]
  (letfn [(good-enough? [guess]
            (< (abs (- (square guess) x)) 0.001))
          (improve [guess]
            (average guess (/ x guess)))]
  ((iterative-improve good-enough? improve) 1.0)))

;;
;; rewrite fixed-point in terms of iterative-improve
;;
(def tolerance 0.00001)

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [guess]
            (< (abs (- guess (f guess))) tolerance))]
    ((iterative-improve close-enough? f) first-guess)))
