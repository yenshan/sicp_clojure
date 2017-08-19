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
    (is (= 'b (exponent (make-exponentiation 'n 'b))))  
    (is (= 'n (base (make-exponentiation 'n 'b))))
    (is (= true (exponentiation? '(** a b))))
    (is (= true (exponentiation? (make-exponentiation 'a 'b))))
  (testing "test deriv + *" 
    (is (= 1 (deriv '(+ x 3) 'x)))
    (is (= 'y (deriv '(* x y) 'x)))
    (is (= '(+ (* x y) (* y (+ x 3))) (deriv '(* (* x y) (+ x 3)) 'x))))
  (testing "test deriv **"
    (is (= 1 (deriv '(+ x 3) 'x)))
    (is (= 'y (deriv '(* x y) 'x)))
    (is (= '(* 3 (** x 2)) (deriv '(** x 3) 'x)))
    (is (= '(* 2 x) (deriv '(** x 2) 'x)))))

