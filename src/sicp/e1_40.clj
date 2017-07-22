(ns sicp.e1_40
  (:require [sicp.newtons-method :refer [newtons-method]]))

(defn- cube [x] (* x x x))
(defn- square [x] (* x x))

(defn cubic [a b c]
  (fn [x] (+ (cube x)
             (* a (square x))
             (* b x)
             c)))

(defn e1_40-test1 []
  (println "compute x^3 + x^2 + x + 1 = 0:"
           (newtons-method (cubic 1 1 1) 1.0)))
