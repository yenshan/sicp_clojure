(ns sicp.e2_1
  (:require [sicp.gcd :refer [gcd]]))

(defn negative? [x] (< x 0))

(defn make-rat [n d] 
  (let [g (gcd (Math/abs n) (Math/abs d))
        n' (/ n g)
        d' (/ d g)]
    (if (negative? d) 
      [(- n') (- d')]
      [n' d']
      ))) 
     
(defn numer [x] (first x))
(defn denom [x] (second x))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat? [x y]
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))



(defn print-rat [x]
  (println (format "%d/%d" (numer x) (denom x))))

(defn e2_1-test []
  (let [one-third (make-rat 1 3)
        three-seven (make-rat 3 7)]
    (print-rat (sub-rat one-third three-seven))))



