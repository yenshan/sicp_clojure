(ns sicp.e2_90.sparse-polynomials
  (:require [clojure.test :refer :all])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [sicp.clj-generic-arithmetie-operations :refer :all]))


;;
;; abstract data: term
;;
(defrecord Term [order coeff])

(defmethod ->list Term [e]
  (list 'Term (:order e) (->list (:coeff e))))

(defn- ->vec [e]
  [(:order e) (:dat (:coeff e))])

(defn- make-num-term [n1 n2]
  (->Term n1 (->SchemeNumber n2)))

;;
;; abstract data: term list
;;

(defn- term->list [coll]
  (map #(->list %) coll))

(defn- term->vec [coll]
  (map #(->vec %) coll))

(def the-empty-termlist '())

(defn- adjoin-term [term term-list]
  (if (=zero? (:coeff term))
    term-list
    (cons term term-list)))

(defn- make-num-termlist [coll]
  (map (fn [[n1 n2]] (make-num-term n1 n2))
       coll))

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

(defn- add-num-terms [L1 L2]
  (add-terms (make-num-termlist L1) (make-num-termlist L2)))

(defn- mul-term-by-all-terms [t1 L]
  (if (empty? L)
    the-empty-termlist
    (let [t2 (first L)]
      (adjoin-term
        (->Term (+ (:order t1) (:order t2))
                (mul (:coeff t1) (:coeff t2)))
        (mul-term-by-all-terms t1 (rest L))))))

(defn- mul-terms [L1 L2]
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
  (testing "test Term"
    (is (= '(Term 2 (SchemeNumber 2) (->list (->Term 2 (->SchemeNumber 2))))))
    (is (= [2 2] (->vec (->Term 2 (->SchemeNumber 2)))))
    )
  (testing "test adjoin"
    (let [tlist (->> the-empty-termlist
                    (adjoin-term (make-num-term 2 2))
                    (adjoin-term (make-num-term 3 3)))]
      (is (= [[3 3] [2 2]] (term->vec tlist)))
    ))
  (testing "test make-num-termlist"
      (is (= [[2 2] [3 3]] (term->vec (make-num-termlist [[2 2] [3 3]]))))
      (is (= [[0 2] [3 3]] (term->vec (make-num-termlist [[0 2] [3 3]]))))
    )
  (testing "test add-terms"
    (is (= [[1 6]] (term->vec (add-terms (make-num-termlist [[1 2]])
                                         (make-num-termlist [[1 4]])))))
    (is (= [[0 6]] (term->vec (add-terms (make-num-termlist [[0 2]])
                                         (make-num-termlist [[0 4]])))))
    (is (= [[2 2] [1 5]] (term->vec (add-terms (make-num-termlist [[2 2] [1 1]])
                                               (make-num-termlist [[1 4]])))))
    (is (= [[2 3] [1 6] [0 1]] (term->vec (add-terms (make-num-termlist [[2 3] [1 2]])
                                                     (make-num-termlist [[1 4] [0 1]])))))
    )
  (testing "test add-num-terms"
    (is (= [[2 3] [1 6] [0 1]] (term->vec (add-num-terms [[2 3] [1 2]]
                                                         [[1 4] [0 1]]))))
    )
  (testing "test add [Polynomial Polynomial]"
  )
  )

