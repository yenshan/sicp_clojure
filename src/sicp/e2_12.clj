(ns sicp.e2_12
  (:require [sicp.e2_11 :refer [->Interval]]))

(defn make-center-percent [center percent]
  (let [width (* center (/ percent 100.0))]
    (->Interval (- center width) (+ center width))))

(defn center [i]
  (/ (+ (:lower i) (:upper i)) 2))

(defn percent [i]
  (-> (- (:upper i) (:lower i))
      (/ 2)
      (/ (center i))
      (Math/abs)
      (* 100)))

(def aa (make-center-percent 60 3))
(center aa)
(percent aa)
(:upper aa)
(:lower aa)

