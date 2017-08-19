(ns sicp.e2_59
  (:require [clojure.test :refer :all]))


(defn variable? [x] (symbol? x))

(defn =number? [exp number]
  (and (number? exp) (= exp number)))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))


(defn addend [x] (first x))
(defn augend [x] (first (rest (rest x))))
(defn sum? [x]
  (and (coll? x) (= (second x) '+)))
(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))

(defn multiplier [x] (first x))
(defn multiplicand [x] (first (rest (rest x))))
(defn product? [x]
  (and (coll? x) (= (second x) '*)))
(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))

(defn base [e] (first e))
(defn exponent [e] (first (rest (rest e))))
(defn exponentiation? [e]
  (and (coll? e) (= (second e) '**)))
(defn make-exponentiation [a1 a2]
  (cond (=number? a2 0) 1
        (=number? a2 1) a1
        :else (list a1 '** a2)))

;; This function doesn't be modified.
(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) variable)
                              (deriv (augend exp) variable))
        (product? exp) (make-sum (make-product (multiplier exp)
                                                 (deriv (multiplicand exp) variable))
                                  (make-product (deriv (multiplier exp) variable)
                                                 (multiplicand exp)))
        (exponentiation? exp)
          (make-product 
            (make-product (exponent exp)
                           (make-exponentiation (base exp) (dec (exponent exp))))
            (deriv (base exp) variable))
        :else (println "unkown expression type -- DERIV" exp)))


(deftest e2_59-test
  (testing "test deriv"
    (is (= true (sum? '(a + b))))
    (is (= 'a (addend '(a + b))))
    (is (= 'b (augend '(a + b))))
    (is (= '(a + b) (make-sum 'a 'b)))
    (is (= true (product? '(a * b))))
    (is (= 'a (multiplier '(a * b))))
    (is (= 'b (multiplicand '(a * b))))
    (is (= '(a * b) (make-product 'a 'b)))
    (is (= true (exponentiation? '(a ** b))))
    (is (= 'a (base '(a ** b))))
    (is (= 'b (exponent '(a ** b))))
    (is (= '(a ** b) (make-exponentiation 'a 'b)))
    (is (= '(x + x) (deriv '(x * x) 'x))
    (is (= '((x * y) + (y * (x + 3))) (deriv '((x * y) * (x + 3)) 'x)))
    )))

