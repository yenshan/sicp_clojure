(ns sicp.sqrt)

(defn abs [x]
  (if (< x 0) (- x) x))

(defn square [x] (* x x))

(defn average [x y] (/ (+ x y) 2))

(defn good-enough? [guess x]
  (< (abs (- (square guess) x)) 0.001))

(defn improve [guess x]
  (average guess (/ x guess)))

(defn sqrt
  ([x] (sqrt 1.0 x))
  ([guess x]
   (if (good-enough? guess x)
     guess
     (recur (improve guess x) x))))
   
