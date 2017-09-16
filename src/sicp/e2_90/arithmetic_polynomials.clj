(ns sicp.e2_90.arithmetic-polynomials
  (:require [clojure.test :refer :all])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [sicp.clj-generic-arithmetie-operations :refer :all])
  (:require [sicp.e2_90.termlist-interface :refer :all])
  (:require [sicp.e2_90.dense-termlist :refer :all])
  (:require [sicp.e2_90.sparse-termlist :refer :all]))

;;
;; arithmetic of polynomial 
;;
(defrecord Polynomial [variable term-list])

(defn- make-sparse-num-polynomial [variable coll]
  (->Polynomial variable (make-sparse-num-termlist coll)))

(defn- make-dense-num-polynomial [variable coll]
  (->Polynomial variable (make-dense-num-termlist coll)))

(defmethod ->vec Polynomial [p]
  (list 'x (->vec (:term-list p))))

(defn variable? [x] (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defmethod add [Polynomial Polynomial] [p1 p2]
  (if (same-variable? (:variable p1) (:variable p2))
    (->Polynomial (:variable p1)
                  (add-terms (:term-list p1)
                             (:term-list p2)))
    (println "Polys not in same var -- ADD-POLY" (list p1 p2))))

(defmethod mul [Polynomial Polynomial] [p1 p2]
  (if (same-variable? (:variable p1) (:variable p2))
    (->Polynomial (:variable p1)
                  (mul-terms (:term-list p1)
                             (:term-list p2)))
    (println "Polys not in same var -- MUL-POLY" (list p1 p2))))

(defmethod =zero? Polynomial [p]
  (=zero? (:term-list p)))

;;
;;
;;
(deftest test-sparse-polynomials
  (testing "test add [Polynomial Polynomial]"
    (is (= '(x [[1 2] [0 1]]) 
           (->vec (add (make-sparse-num-polynomial 'x [[1 1]])
                       (make-sparse-num-polynomial 'x [[1 1] [0 1]])))))
    (is (= '(x [2 2]) 
           (->vec (add (make-dense-num-polynomial 'x [1 1])
                       (make-dense-num-polynomial 'x [1 1])))))
                                
  )
  (testing "test mul [Polynomial Polynomial]"
    (is (= '(x [[2 1] [1 1]])
           (->vec (mul (make-sparse-num-polynomial 'x [[1 1]])
                       (make-sparse-num-polynomial 'x [[1 1] [0 1]])))))
    (is (= '(x [1 2 1]) 
           (->vec (mul (make-dense-num-polynomial 'x [1 1])
                       (make-dense-num-polynomial 'x [1 1])))))

    )
  (testing "test =zero?"
    (is (=zero? (make-sparse-num-polynomial 'x [[2 0]])))
    (is (not (=zero? (make-sparse-num-polynomial 'x [[0 2]]))))
    )
  )

