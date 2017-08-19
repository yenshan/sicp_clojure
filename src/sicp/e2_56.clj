(ns sicp.e2_56
  (:require [sicp.symbolic-differentiation :refer :all])
  (:require [clojure.test :refer :all]))

(defn make-exponentiation [a1 a2]
  (cond (=number? a2 0) 1
        (=number? a2 1) a1
        :else (list '** a1 a2)))

(defn base [e] (first (rest e)))
(defn exponent [e] (first (rest (rest e))))
(defn exponentiation? [e]
  (and (coll? e) (= (first e) '**)))

(defn deriv3 [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0)
        (sum? exp) (make-sum2 (deriv3 (addend exp) variable)
                              (deriv3 (augend exp) variable))
        (product? exp) (make-sum2 (make-product2 (multiplier exp)
                                                 (deriv3 (multiplicand exp) variable))
                                  (make-product2 (deriv3 (multiplier exp) variable)
                                                 (multiplicand exp)))
        (exponentiation? exp)
          (make-product2 
            (make-product2 (exponent exp)
                           (make-exponentiation (base exp) (dec (exponent exp))))
            (deriv3 (base exp) variable))
        :else (println "unkown expression type -- DERIV" exp)))

(deftest e2_56-test
  (testing "test drive"
    (is (= 1 (deriv3 '(+ x 3) 'x)))
    (is (= 'y (deriv3 '(* x y) 'x)))
    (is (= 'b (exponent (make-exponentiation 'n 'b))))  
    (is (= 'n (base (make-exponentiation 'n 'b))))
    (is (= true (exponentiation? '(** a b))))
    (is (= true (exponentiation? (make-exponentiation 'a 'b))))
    (is (= '(* 3 (** x 2)) (deriv3 '(** x 3) 'x)))
    (is (= '(* 2 x) (deriv3 '(** x 2) 'x)))
    ))

