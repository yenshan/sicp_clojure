(ns sicp.e2_57
  (:require [clojure.test :refer :all])
  (:require [sicp.symbolic-differentiation :refer [variable? same-variable? =number?
                                                   sum? addend product? multiplier
                                                   make-sum make-product
                                                   make-exponentiation 
                                                   exponentiation? exponent base]]))


;(defn make-sum [a1 a2 & coll]
;  (if (nil? coll)
;    (cond (=number? a1 0) a2
;          (=number? a2 0) a1
;          (and (number? a1) (number? a2)) (+ a1 a2)
;          :else (list '+ a1 a2))
;    (make-sum a1 (apply make-sum a2 (first coll) (rest coll)))))
;
;  
;(defn make-product [m1 m2 & coll]
;  (if (nil? coll)
;    (cond (or (=number? m1 0) (=number? m2 0)) 0
;          (=number? m1 1) m2
;          (=number? m2 1) m1
;          (and (number? m1) (number? m2)) (* m1 m2)
;          :else (list '* m1 m2))
;    (make-product m1 (apply make-product m2 (first coll) (rest coll)))))

(defn multiplicand [x]
  (let [op (first x)
        m (rest (rest x))]
    (if (empty? (rest m))
      (first m)
      (cons op m))))

(defn augend [x]
  (let [op (first x)
        m (rest (rest x))]
    (if (empty? (rest m))
      (first m)
      (cons op m))))

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

(deftest e2_57-test
  (testing "test deriv"
;    (is (= '(+ a (+ b (+ c d))) (make-sum 'a 'b 'c 'd)))
;    (is (= '(* a (* b c)) (make-product 'a 'b 'c)))
    (is (= '(+ (* x (+ x x)) (* x x)) (deriv '(* x x x) 'x)))
    (is (= '(+ (* x y) (* y (+ x 3))) (deriv '(* x y (+ x 3)) 'x)))
    ))

