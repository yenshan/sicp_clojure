(ns sicp.message-passing
  (:require [clojure.test :refer :all])
  (:require [clojure.algo.generic.math-functions :as math]))

(defn square [x] (* x x))

(defn make-from-real-imag [x y]
  (letfn [(dispatch [op]
            (cond (= op 'real-part) x
                  (= op 'imag-part) y
                  (= op 'magnitude) (Math/sqrt (+ (square x) (square y)))
                  (= op 'angle) (math/atan2 y x)
                  (= op 'data) (list 'rectangular (list x y))
                  :else (println "Unkown op -- MAKE-FROM-REAL-IMAG" op)))]
    dispatch))

(defn make-from-mag-ang [r a]
  (letfn [(dispatch [op]
            (cond (= op 'real-part) (* r (math/cos a))
                  (= op 'imag-part) (* r (math/sin a))
                  (= op 'magnitude) r
                  (= op 'angle) a
                  (= op 'data) (list 'polar (list r a))
                  :else (println "Unkown op -- MAKE-FROM-MAG-ANG" op)))]
    dispatch))

          
(defn apply-generic [op arg] (arg op))
                  
(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))

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


(defn get-data [z] (z 'data))

(deftest test-message_passing
  (testing "test complex calculation"
    (let [one (make-from-real-imag 1 1)
          two (make-from-real-imag 2 1)]
      (is (= '(rectangular (3 2)) (get-data (add-complex one two))))
      (is (= '(rectangular (-1 0)) (get-data (sub-complex one two))))
      (is (= '(polar (3.1622776601683795 0.3641479805728513)) (get-data (mul-complex one two))))
      (is (= '(polar (0.6324555320336759 0.3217505543966422)) (get-data (div-complex one two))))
      )))

