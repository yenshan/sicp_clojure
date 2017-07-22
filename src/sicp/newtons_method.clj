(ns sicp.newtons-method
  (:require [sicp.fixed-point :refer [fixed-point]]))

(def dx 0.00001)

(defn deriv [g]
  (fn [x] (/ (- (g (+ x dx)) (g x))
             dx)))
    
(defn newton-transform [g]
  (fn [x] (- x (/ (g x) ((deriv g) x)))))

(defn newtons-method [g guess]
  (fixed-point (newton-transform g) guess))

(defn square [x] (* x x))

(defn sqrt [x]
  (newtons-method (fn [y] (- (square y) x))
                  1.0))

