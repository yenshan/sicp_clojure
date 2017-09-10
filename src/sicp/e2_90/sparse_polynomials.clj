(ns sicp.e2_90.sparse-polynomials
  (:require [clojure.test :refer :all])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [sicp.clj-generic-arithmetie-operations :refer :all]))

;;
;; abstract data: term
;;
(defrecord Term [order coeff])

;;
;; abstract data: term list
;;
(defrecord TermList [dat])

(def the-empty-termlist (->TermList '()))

(defn- adjoin-term [term term-list]
  (if (=zero? (:coeff term))
    term-list
    (->TermList (cons term (:dat term-list)))))

;;
;; Term arithmetic operations
;;
(defn- add-terms [L1 L2]
  (cond (empty? L1) L2
        (empty? L2) L1
        :else 
         (let [t1 (first L1)
               t2 (first L2)]
          (cond (> (:order t1) (:order t2)) 
                 (adjoin-term t1
                              (add-terms (rest L1) L2))
                (< (:order t1) (:order t2)) 
                 (adjoin-term t2
                              (add-terms L1 (rest L2)))
                :else 
                 (adjoin-term (->Term (:order t1)
                                      (add (:coeff t1) (:coeff t2)))
                              (add-terms (rest L1)
                                         (rest L2)))))))
(defn- mul-term-by-all-terms [t1 L]
  (if (empty? L)
    the-empty-termlist
    (let [t2 (first L)]
      (adjoin-term
        (->Term (+ (:order t1) (:order t2))
                (mul (:coeff t1) (:coeff t2)))
        (mul-term-by-all-terms t1 (rest L))))))

(defmethod mul [TermList TermList] [L1 L2]
  (if (empty? L1)
    the-empty-termlist
    (add (mul-term-by-all-terms (first L1) L2)
         (mul (rest L1) L2))))
      
;;
;; arithmetic of polynomial 
;;
(defrecord Polynomial [variable term-list])

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defmethod add [Polynomial Polynomial] [p1 p2]
  (if (same-variable? (:variable p1) (:variable p2))
    (->Polynomial (:variable p1)
                  (add (:term-list p1)
                       (:term-list p2)))
    (println "Polys not in same var -- ADD-POLY" (list p1 p2))))

(defmethod mul [Polynomial Polynomial] [p1 p2]
  (if (same-variable? (:variable p1) (:variable p2))
    (->Polynomial (:variable p1)
                  (mul (:term-list p1)
                       (:term-list p2)))
    (println "Polys not in same var -- MUL-POLY" (list p1 p2))))

(defmethod =zero? Polynomial [p]
  (let [coeff-not-zero? (comp not =zero? :coeff)]
    (->> (:term-list p)
         (some coeff-not-zero?)
         (= nil))))

;;
;;
;;
(deftest test-sparse-polynomials
  (testing "test =zero?"
    (is (=zero? (->Polynomial 'x (list (->Term 2 (->SchemeNumber 0))))))
    )
  (testing "test add [TermList TermList]"
    (let [term1 (->Term 2 (->SchemeNumber 2))
          tl1 (adjoin-term term1 the-empty-termlist)]
    ))
  )
