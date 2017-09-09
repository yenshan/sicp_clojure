(ns sicp.arithmetic_polynomials
  (:require [clojure.test :refer :all])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all]))

;;
;; abstract data: term
;;
(defn make-term [order coeff] (list order coeff))
(defn order [term] (first term))
(defn coeff [term] (second term))

;;
;; abstract data: term list
;;
(def the-empty-termlist '())
(defn first-term [term-list] (first term-list))
(defn rest-terms [term-list] (rest term-list))
(defn empty-termlist? [term-list] (empty? term-list))

(defn adjoin-term [term term-list]
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

(defn add-terms [L1 L2]
  (cond (empty-termlist? L1) L2
        (empty-termlist? L2) L1
        :else 
         (let [t1 (first-term L1)
               t2 (first-term L2)]
          (cond (> (order t1) (order t2)) 
                 (adjoin-term t1
                              (add-terms (rest-terms L1) L2))
                (< (order t1) (order t2)) 
                 (adjoin-term t2
                              (add-terms L1 (rest-terms L2)))
                :else 
                 (adjoin-term (make-term (order t1)
                                         (add (coeff t1) (coeff t2)))
                              (add-terms (rest-terms L1)
                                         (rest-terms L2)))))))
(defn mul-term-by-all-terms [t1 L]
  (if (empty-termlist? L)
    the-empty-termlist
    (let [t2 (first-term L)]
      (adjoin-term
        (make-term (+ (order t1) (order t2))
                   (mul (coeff t1) (coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

(defn mul-terms [L1 L2]
  (if (empty-termlist? L1)
    the-empty-termlist
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))
      
;;
;; arithmetic of polynomial 
;;
(defn- make-poly [variable term-list]
  (list variable term-list))

(defn variable [p] (first p))
(defn term-list [p] (second p))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn add-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (add-terms (term-list p1)
                          (term-list p2)))
    (println "Polys not in same var -- ADD-POLY" (list p1 p2))))

(defn mul-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (mul-terms (term-list p1)
                          (term-list p2)))
    (println "Polys not in same var -- MUL-POLY" (list p1 p2))))


(defn- tag [p] (attach-tag 'polynomial p))

(defn install-polynomial-package []
    (tput 'add '(polynomial polynomial)
      (fn [p1 p2] (tag (add-poly p1 p2))))
    (tput 'mul '(polynomial polynomial)
      (fn [p1 p2] (tag (mul-poly p1 p2))))
    (tput 'make 'polynomial
      (fn [var terms] (tag (make-poly var terms)))))


;;
;; Tests
;;
(deftest test-arithmetic-polynomials
  (let [_ (install-scheme-number-package)]
    (testing "test adjoin-term"
      (is (= '((2 (scheme-number 3))) 
             (adjoin-term (make-term 2 (make-scheme-number 3))
                          the-empty-termlist)))
      )
    (testing "test add-terms"
      (is (= '((2 (scheme-number 5))
               (add-terms '((2 (scheme-number 3))) '((2 (scheme-number 2)))))))
      (is (= '((2 (scheme-number 3)) (1 (scheme-number 1)))
               (add-terms '((2 (scheme-number 3))) '((1 (scheme-number 1))))))
      )
    (testing "test mul-terms"
      (is (= '((3 (scheme-number 3))) 
               (mul-terms '((2 (scheme-number 3))) '((1 (scheme-number 1))))))
      )
    )
  )
