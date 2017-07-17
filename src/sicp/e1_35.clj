(ns sicp.e1_35
  (:require [sicp.fixed-point :refer [fixed-point]]))

(def golden-ratio 
  (fixed-point #(+ 1 (/ 1 %)) 1.0))

(defn test1 []
  (println "golden-ration is " golden-ratio))

