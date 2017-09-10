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
(defn- first-term [term-list] (first (:dat term-list)))
(defn- rest-terms [term-list] (->TermList (rest (:dat term-list))))
(defn- empty-termlist? [term-list] (empty? (:dat term-list)))

(defn- adjoin-term [term term-list]
  (if (=zero? (:coeff term))
    term-list
    (->TermList (cons term (:dat term-list)))))

;;
;; Term arithmetic operations
;;
(defmethod add [TermList TermList] [L1 L2]
  (cond (empty-termlist? L1) L2
        (empty-termlist? L2) L1
        :else 
         (let [t1 (first-term L1)
               t2 (first-term L2)]
          (cond (> (:order t1) (:order t2)) 
                 (adjoin-term t1
                              (add (rest-terms L1) L2))
                (< (:order t1) (:order t2)) 
                 (adjoin-term t2
                              (add L1 (rest-terms L2)))
                :else 
                 (adjoin-term (->Term (:order t1)
                                         (add (:coeff t1) (:coeff t2)))
                              (add (rest-terms L1)
                                         (rest-terms L2)))))))
(defn- mul-term-by-all-terms [t1 L]
  (if (empty-termlist? L)
    the-empty-termlist
    (let [t2 (first-term L)]
      (adjoin-term
        (->Term (+ (:order t1) (:order t2))
                (mul (:coeff t1) (:coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

(defmethod mul [TermList TermList] [L1 L2]
  (if (empty-termlist? L1)
    the-empty-termlist
    (add (mul-term-by-all-terms (first-term L1) L2)
         (mul (rest-terms L1) L2))))
      
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

(defmethod =zero? [Polynomial] [p]
  (letfn [(coeff-not-zero? [term]
            (not (=zero? (:coeff term))))]
    (->> (:term-list p)
         (some coeff-not-zero?)
         (= nil))))

(defn make-polynomial [var terms]
  (->Polynomial var terms))


;;
;;
;;
(deftest test-sparse-polynomials
  (testing "test add [TermList TermList]"
    (let [term1 (->Term 2 (->SchemeNumber 2))
          tl1 (adjoin-term term1 the-empty-termlist)]
    ))
  )
