(ns sicp.symbolic-differentiation-test
  (:require [clojure.test :refer :all]
            [sicp.symbolic-differentiation :refer :all]))

(deftest symbolic-differentiation-test
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
    (is (= '(+ (* x 0) (* 1 y)) (deriv '(* x y) 'x))))
  (testing "test: sicp.symbolic-differentiation: deriv2"
    (is (= 1 (deriv2 '(+ x 3) 'x)))
    (is (= 'y (deriv2 '(* x y) 'x)))
    (is (= '(+ (* x y) (* y (+ x 3))) (deriv2 '(* (* x y) (+ x 3)) 'x)))))

