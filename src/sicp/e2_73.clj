(ns sicp.e2_73
  (:require [clojure.test :refer :all]))

(def table-operations (atom {}))

(defn tput [key1 key2 func]
  (swap! table-operations assoc-in [key1 key2] func))

(defn tget [key1 key2]
  (get-in @table-operations [key1 key2]))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp number]
  (and (number? exp) (= exp number)))

(defn install-deriv-package []
  (letfn [
          (addend [x] (first x))
          (augend [x] (second x))
          (multiplier [x] (first x))
          (multiplicand [x] (second x))
          (base [e] (first e))
          (exp [e] (second e))
          (make-sum [a1 a2]
            (cond (=number? a1 0) a2
                  (=number? a2 0) a1
                  (and (number? a1) (number? a2)) (+ a1 a2)
                  :else (list '+ a1 a2)))
          (make-product [m1 m2]
            (cond (or (=number? m1 0) (=number? m2 0)) 0
                  (=number? m1 1) m2
                  (=number? m2 1) m1
                  (and (number? m1) (number? m2)) (* m1 m2)
                  :else (list '* m1 m2)))
          (make-exponentiation [a1 a2]
            (cond (=number? a2 0) 1
                  (=number? a2 1) a1
                  :else (list '** a1 a2)))
          (sum [operands variable]
            (make-sum (deriv (addend operands) variable)
                      (deriv (augend operands) variable)))
          (product [operands variable]
            (make-sum (make-product (multiplier operands)
                                    (deriv (multiplicand operands) variable))
                      (make-product (deriv (multiplier operands) variable)
                                    (multiplicand operands))))
          (exponent [operands variable]
            (make-product 
              (make-product (exp operands)
                            (make-exponentiation (base operands) (dec (exp operands))))
              (deriv (base operands) variable)))
          ]
    (tput 'deriv '+ sum)
    (tput 'deriv '* product) 
    (tput 'deriv '** exponent) 
    ))

(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))

(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0) 
        :else ((tget 'deriv (operator exp))
               (operands exp) variable)))

(deftest test-e2-73
  (testing "test operator, operands"
    (is (= '+ (operator '(+ 1 2))))
    (is (= '(1 2) (operands '(+ 1 2))))
    )
  (testing "test deriv"
    (let [_ (install-deriv-package)]
      (is (= 1 (deriv '(+ x 3) 'x)))
      (is (= 'y (deriv '(* x y) 'x)))
      (is (= '(* 2 x) (deriv '(** x 2) 'x)))
      )
    ))
