(ns sicp.e2_56
  (:require [clojure.test :refer :all])
  (:require [sicp.symbolic-differentiation :refer [variable? same-variable? sum? make-sum addend augend product? make-product multiplier multiplicand =number?]]))

(defn make-exponentiation [a1 a2]
  (cond (=number? a2 0) 1
        (=number? a2 1) a1
        :else (list '** a1 a2)))

(defn base [e] (first (rest e)))
(defn exponent [e] (first (rest (rest e))))
(defn exponentiation? [e]
  (and (coll? e) (= (first e) '**)))

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

(deftest e2_56-test
  (testing "test deriv"
    (is (= 1 (deriv '(+ x 3) 'x)))
    (is (= 'y (deriv '(* x y) 'x)))
    (is (= 'b (exponent (make-exponentiation 'n 'b))))  
    (is (= 'n (base (make-exponentiation 'n 'b))))
    (is (= true (exponentiation? '(** a b))))
    (is (= true (exponentiation? (make-exponentiation 'a 'b))))
    (is (= '(* 3 (** x 2)) (deriv '(** x 3) 'x)))
    (is (= '(* 2 x) (deriv '(** x 2) 'x)))
    ))

