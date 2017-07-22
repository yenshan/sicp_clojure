(ns sicp.average-damp
  (:require [sicp.fixed-point :refer [fixed-point]]))

(defn average [x y] (/ (+ x y) 2))

(defn average-damp [f]
  (fn [x] (average x (f x))))


(defn- sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1.0))
    
(defn test1 []
  (println "sqrt 2 is " (sqrt 2)))
