(ns sicp.e2_8
  (:require [sicp.e2_7 :refer :all]))


(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(def aa (make-interval 10 12))
(def bb (make-interval 13 15))

(sub-interval a b)
