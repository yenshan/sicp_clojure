(ns sicp.e2_75
  (:require [clojure.test :refer :all])
  (:require [clojure.algo.generic.math-functions :as math]))

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

(deftest test-e2_75
  (testing "test make-from-mag-ang"
    (is (= -0.4161468365471424 (real-part (make-from-mag-ang 1 2))))
    (is (= 0.9092974268256817 (imag-part (make-from-mag-ang 1 2))))
    (is (= 1 (magnitude (make-from-mag-ang 1 2))))
    (is (= 2 (angle (make-from-mag-ang 1 2))))
    ))    

