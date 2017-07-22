(ns sicp.e1_44
  (:require [sicp.e1_43 :refer [repeated]]))

(defn average [x y z]
  (/ (+ x y z) 3))

(def dx 0.00001)

(defn smooth [f]
  (fn [x] (average (f (- x dx))
                   (f x)
                   (f (+ x dx)))))
                      
(defn n-fold-smooth [f n]
  ((repeated smooth n) f))
