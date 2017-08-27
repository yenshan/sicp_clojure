(ns sicp.generic-arithmetie-operations
  (:require [clojure.test :refer :all])
  (:require [sicp.gcd :refer [gcd]])
  (:require [sicp.complex-number :refer [real-part imag-part magnitude angle]])
  (:require [sicp.data-directed-lib :refer :all]))

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

(defn install-scheme-number-package []
  (letfn [(tag [x]
            (attach-tag 'scheme-number x))]
    (tput 'add '(scheme-number schem-number)
      (fn [x y] (tag (+ x y))))
    (tput 'sub '(scheme-number schem-number)
      (fn [x y] (tag (- x y))))
    (tput 'mul '(scheme-number schem-number)
      (fn [x y] (tag (* x y))))
    (tput 'div '(scheme-number schem-number)
      (fn [x y] (tag (/ x y))))
    (tput 'make 'scheme-number
      (fn [x] (tag x))))
  )

(defn make-scheme-number [n]
  ((tget 'make 'scheme-number) n))

(defn install-rational-package []
  (letfn [(numer [x] (first x))
          (denom [x] (second x))
          (make-rat [n d] 
            (let [g (gcd n d)]
              [(/ n g) (/ d g)]))
          (add-rat [x y]
            (make-rat (+ (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (sub-rat [x y]
            (make-rat (- (* (numer x) (denom y))
                         (* (numer y) (denom x)))
                      (* (denom x) (denom y))))
          (mul-rat [x y]
            (make-rat (* (numer x) (numer y))
                      (* (denom x) (denom y))))
          (div-rat [x y]
            (make-rat (* (numer x) (denom y))
                      (* (denom x) (numer y))))
          (tag [x]
            (attach-tag 'rational x))
          ]
    (tput 'add '(rational rational)
      (fn [x y] (tag (add-rat x y))))
    (tput 'sub '(rational rational)
      (fn [x y] (tag (sub-rat x y))))
    (tput 'mul '(rational rational)
      (fn [x y] (tag (mul-rat x y))))
    (tput 'div '(rational rational)
      (fn [x y] (tag (div-rat x y))))
    (tput 'make 'rational
      (fn [n d] (tag (make-rat n d))))
    ))

(defn make-rational [n d]
  ((tget 'make 'rational) n d))


(defn install-complex-package []
  (letfn [
          (make-from-real-imag [x y]
            ((tget 'make-from-real-imag 'rectangular) x y))
          (make-from-mag-ang [r a]
            ((tget 'make-from-mag-ang 'polar) r a))
          (add-complex [z1 z2]
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2))))
          (sub-complex [z1 z2]
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2))))
          (mul-complex [z1 z2]
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (* (angle z1) (angle z2))))
          (div-complex [z1 z2]
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2))))
          (tag [x]
            (attach-tag 'complex x))
          ]
    (tput 'add '(complex complex)
      (fn [x y] (tag (add-complex x y))))
    (tput 'sub '(complex complex)
      (fn [x y] (tag (sub-complex x y))))
    (tput 'mul '(complex complex)
      (fn [x y] (tag (mul-complex x y))))
    (tput 'div '(complex complex)
      (fn [x y] (tag (div-complex x y))))
    (tput 'make-from-real-imag 'complex
      (fn [x y] (tag (make-from-real-imag x y))))
    (tput 'make-from-mag-ang 'complex
      (fn [r a] (tag (make-from-mag-ang r a))))
    ))

(defn make-complex-from-real-imag [x y]
  ((tget 'make-from-mag-ang 'complex) x y))

(defn make-complex-from-mag-ang [r a]
  ((tget 'make-from-mag-ang 'complex) r a))
