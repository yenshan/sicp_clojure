(ns sicp.symbolic-differentiation00
  (:require [clojure.test :refer :all]))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2] (list '+ a1 a2))

(defn make-product [m1 m2] (list '* m1 m2))

(defn sum? [x]
  (and (coll? x) (= (first x) '+)))

(defn addend [x] (first (rest x)))

(defn augend [x] (first (rest (rest x))))

(defn product? [x]
  (and (coll? x) (= (first x) '*)))

(defn multiplier [x] (first (rest x)))

(defn multiplicand [x] (first (rest (rest x))))


(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) variable)
                             (deriv (augend exp) variable))
        (product? exp) (make-sum (make-product (multiplier exp)
                                               (deriv (multiplicand exp) variable))
                                 (make-product (deriv (multiplier exp) variable)
                                               (multiplicand exp)))
        :else (println "unkown expression type -- DERIV" exp)))


(deftest test-symbolic-differentiation00
  (testing "test: sicp.symbolic-differentiation: primitives."
    (is (same-variable? 'x 'x))
    (is (not (same-variable? 'x 'y)))
    (is (sum? (make-sum 'a 'b)))
    (is (not (sum? (make-product 'a 'b))))
    (is (not (product? (make-sum 'a 'b))))
    (is (product? (make-product 'a 'b)))
    (is (= 'a (addend (make-sum 'a 'b)))) 
    (is (= 'b (augend (make-sum 'a 'b)))) 
    (is (= 'a (multiplier '(* a b))))
    (is (= 'b (multiplicand '(* a b)))))
  (testing "test: sicp.symbolic-differentiation: deriv"
    (is (= '(+ 1 0) (deriv '(+ x 3) 'x)))
    (is (= '(+ (* x 0) (* 1 y)) (deriv '(* x y) 'x)))))
