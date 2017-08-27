(ns sicp.complex-number
  (:require [clojure.test :refer :all])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [clojure.algo.generic.math-functions :as math]))

(defn square [x] (* x x))

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (magnitude [z]
            (Math/sqrt (+ (square (real-part z))
                          (square (imag-part z)))))
          (angle [z]
            (math/atan2 (imag-part z) (real-part z)))
          (make-from-real-imag [x y] (list x y))
          (make-from-mag-ang [r a]
            (list (* r (math/cos a)) (* r (math/sin a))))
          ;;
          (tag [x] (attach-tag 'rectangular x))]

    (tput 'real-part '(rectangular) real-part)
    (tput 'imag-part '(rectangular) imag-part)
    (tput 'magnitude '(rectangular) magnitude)
    (tput 'angle '(rectangular) angle)
    (tput 'make-from-real-imag 'rectangular
      (fn [x y] (tag (make-from-real-imag x y))))
    (tput 'make-from-mag-ang 'rectangular
      (fn [r a] (tag (make-from-mag-ang r a))))
    ))

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (real-part [z]
            (* (magnitude z) (math/cos (angle z))))
          (imag-part [z]
            (* (magnitude z) (math/sin (angle z))))
          (make-from-real-imag [x y]
            (list (Math/sqrt (+ (square x) (square y)))
                  (math/atan2 y x)))
          (make-from-mag-ang [r a] (list r a))
          ;;
          (tag [x] (attach-tag 'polar x))]

    (tput 'real-part '(polar) real-part)
    (tput 'imag-part '(polar) imag-part)
    (tput 'magnitude '(polar) magnitude)
    (tput 'angle '(polar) angle)
    (tput 'make-from-real-imag 'polar
      (fn [x y] (tag (make-from-real-imag x y))))
    (tput 'make-from-mag-ang 'polar
      (fn [r a] (tag (make-from-mag-ang r a))))
    ))

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))
(defn make-from-real-imag [x y]
  ((tget 'make-from-real-imag 'rectangular) x y))
(defn make-from-mag-ang [r a]
  ((tget 'make-from-mag-ang 'polar) r a))

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


(deftest test-complex-number
  (testing "tput, tget"
    (let [_ (tput 'a 'b 1)
          _ (tput 'c '(d) 2)]
      (is (= 1 (tget 'a 'b))) 
      (is (= 2 (tget 'c '(d)))) 
    ))
  (testing "install-rectangular-package"
    (let [_ (install-rectangular-package)
          real-part (tget 'real-part '(rectangular))
          imag-part (tget 'imag-part '(rectangular))
          magnitude (tget 'magnitude '(rectangular))
          angle (tget 'angle '(rectangular))
          make-from-real-imag (tget 'make-from-real-imag 'rectangular)
          make-from-mag-ang (tget 'make-from-mag-ang 'rectangular)
          ]
      (is (= 'a (real-part '(a b))))
      (is (= 'b (imag-part '(a b))))
      (is (= 1.4142135623730951 (magnitude (contents (make-from-real-imag 1 1)))))
      (is (= 0.7853981633974483 (angle (contents (make-from-real-imag 1 1)))))
      (is (= '(rectangular (a b)) (make-from-real-imag 'a 'b)))
      (is (= '(rectangular (0.15425144988758405 -0.9880316240928618))
             (make-from-mag-ang 1 30)))
      ))
  (testing "test complex calculation"
    (let [_ (install-rectangular-package)
          _ (install-polar-package)
          one (make-from-real-imag 1 1)
          two (make-from-real-imag 2 1)]
      (is (= '(rectangular (3 2)) (add-complex one two)))
      (is (= '(rectangular (-1 0)) (sub-complex one two)))
      (is (= '(polar (3.1622776601683795 0.3641479805728513)) (mul-complex one two)))
      (is (= '(polar (0.6324555320336759 0.3217505543966422)) (div-complex one two)))
      ))
    )
