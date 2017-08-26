;;
;; Implementation of Complex Number in Clojure style
;;
(ns sicp.clj-complex-number
  (:require [clojure.test :refer :all])
  (:require [clojure.algo.generic.math-functions :as math]))

(defn square [x] (* x x))

(defrecord Rectangular [real imag])
(defrecord Polar [magnitude angle]) 

(defmulti real-part (fn [dat] (class dat)))
(defmulti imag-part (fn [dat] (class dat)))
(defmulti magnitude (fn [dat] (class dat)))
(defmulti angle (fn [dat] (class dat)))
(defmulti -make-from-real-imag (fn [t x y] t)) 
(defmulti -make-from-mag-ang (fn [t x y] t)) 

;;
;; functions for Retangular 
;;
(defmethod real-part Rectangular [z] (:real z))

(defmethod imag-part Rectangular [z] (:imag z))

(defmethod magnitude Rectangular [z]
  (Math/sqrt (+ (square (real-part z))
                (square (imag-part z)))))

(defmethod angle Rectangular [z]
  (math/atan2 (imag-part z) (real-part z)))

(defmethod -make-from-real-imag Rectangular [t x y]
  (->Rectangular x y))

(defmethod -make-from-mag-ang Rectangular [t r a]
  (->Rectangular (* r (math/cos a))
                 (* r (math/sin a))))

;;
;; functions for Polar
;;
(defmethod real-part Polar [z]
  (* (magnitude z) (math/cos (angle z))))

(defmethod imag-part Polar [z] 
  (* (magnitude z) (math/sin (angle z))))

(defmethod magnitude Polar [z]
  (:magnitude z))

(defmethod angle Polar [z]
  (:angle z))

(defmethod -make-from-real-imag Polar [t x y]
  (->Polar (Math/sqrt (+ (square x) (square y)))
           (math/atan2 y x)))

(defmethod -make-from-mag-ang Polar [t r a]
  (->Polar r a))

;;
;; functions for Complex Number
;;
(defn make-from-real-imag [x y]
  (-make-from-real-imag Rectangular x y))

(defn make-from-mag-ang [r a]
  (-make-from-mag-ang Polar r a))

(defn add-complex [z1 z2]
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(defn sub-complex [z1 z2]
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(defn mul-complex [z1 z2]
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (* (angle z1) (angle z2))))

(defn div-complex [z1 z2]
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;;
;; tests
;;
(deftest test-complex-number
  (testing "test make-from-real-imag"
    (is (= 1 (real-part (-make-from-real-imag Rectangular 1 2))))
    (is (= 2 (imag-part (-make-from-real-imag Rectangular 1 2))))
    )
  (testing "test complex calculation"
    (let [one (make-from-real-imag 1 1)
          two (make-from-real-imag 2 1)]
      (is (= (->Rectangular 3 2)) (add-complex one two))
      (is (= (->Rectangular -1 0) (sub-complex one two)))
      (is (= (->Polar 3.1622776601683795 0.3641479805728513) (mul-complex one two)))
      (is (= (->Polar 0.6324555320336759 0.3217505543966422) (div-complex one two)))
      ))
    )
