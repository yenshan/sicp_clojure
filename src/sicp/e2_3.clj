(ns sicp.e2_3
  (:require [sicp.e2_2 :refer [make-point x-point y-point 
                               make-segment start-segment end-segment]]))

(defn double- [x] (* x x))

(defn make-rect [p1 p2 p3 p4] [p1 p2 p3 p4])

(defn left-edge [x]
  (make-segment (get x 0) (get x 1)))

(defn bottom-edge [x]
  (make-segment (get x 1) (get x 2)))

(defn right-edge [x]
  (make-segment (get x 2) (get x 3)))

(defn top-edge [x]
  (make-segment (get x 3) (get x 0)))

(defn length [p1 p2]
  (Math/sqrt (+ (double- (- (x-point p1) (x-point p2)))
                (double- (- (y-point p1) (y-point p2))))))

(defn len-segment [x]
  (length (start-segment x) (end-segment x)))

(defn perimeter-rect [x]
  (reduce + (map len-segment [(left-edge x)
                              (bottom-edge x)
                              (right-edge x)
                              (top-edge x)])))
(defn area-rect [x]
  (* (len-segment (left-edge x))
     (len-segment (bottom-edge x))))


(def p1 (make-point 0 0))
(def p2 (make-point 0 10))
(def p3 (make-point 10 10))
(def p4 (make-point 10 0))
(def rect1 (make-rect p1 p2 p3 p4))

(defn e2_3-test []
  (println "left edge length =" (len-segment (left-edge rect1))) 
  (println "perimeter =" (perimeter-rect rect1))
  (println "area =" (area-rect rect1)))


