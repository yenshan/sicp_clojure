(ns sicp.e2_90.sparse-termlist
  (:require [clojure.test :refer :all])
  (:require [sicp.clj-generic-arithmetie-operations :refer :all])
  (:require [sicp.e2_90.termlist-interface :refer :all]))

;;
;; abstract data: term
;;
(defrecord Term [order coeff])

(defmethod ->list Term [e]
  (list 'Term (:order e) (->list (:coeff e))))

(defmethod ->vec Term [e]
  [(:order e) (:dat (:coeff e))])

(defn- make-num-term [n1 n2]
  (->Term n1 (->SchemeNumber n2)))

;;
;; abstract data: term list
;;
(defrecord SparseTermList [dat])

(defmethod constructor SparseTermList [L]
  (fn [dat] (->SparseTermList dat)))

(defmethod ->vec SparseTermList [coll]
  (map #(->vec %) (:dat coll)))

(def ^:private the-empty-termlist (->SparseTermList '()))

(defn- adjoin-term [term term-list]
  (if (=zero? (:coeff term))
    term-list
    (->SparseTermList (cons term (:dat term-list)))))

(defmethod add-terms [SparseTermList SparseTermList] [L1 L2]
  (cond (empty-term? L1) L2
        (empty-term? L2) L1
        :else (let [t1 (first-term L1)
                    t2 (first-term L2)]
                (cond (> (:order t1) (:order t2)) 
                       (adjoin-term t1
                                    (add-terms (rest-terms L1) L2))
                      (< (:order t1) (:order t2)) 
                       (adjoin-term t2
                                    (add-terms L1 (rest-terms L2)))
                      :else 
                       (adjoin-term (->Term (:order t1)
                                            (add (:coeff t1) (:coeff t2)))
                                    (add-terms (rest-terms L1)
                                               (rest-terms L2)))))))

(defn make-sparse-num-termlist [coll]
  (->SparseTermList 
    (map (fn [[n1 n2]] (make-num-term n1 n2))
         coll)))

(defn- add-sparse-num-terms [L1 L2]
  (add-terms (make-sparse-num-termlist L1) (make-sparse-num-termlist L2)))

(defn- mul-term-by-all-terms [t1 L]
  (if (empty-term? L)
    the-empty-termlist
    (let [t2 (first-term L)]
      (adjoin-term
        (->Term (+ (:order t1) (:order t2))
                (mul (:coeff t1) (:coeff t2)))
        (mul-term-by-all-terms t1 (rest-terms L))))))

(defmethod mul-terms [SparseTermList SparseTermList] [L1 L2]
  (if (empty-term? L1)
    the-empty-termlist
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))

(defn- mul-num-terms [L1 L2]
  (mul-terms (make-sparse-num-termlist L1) (make-sparse-num-termlist L2)))

(defmethod =zero? SparseTermList [L]
  (let [coeff-not-zero? (comp not =zero? :coeff)]
    (->> (:dat L)
         (some coeff-not-zero?)
         (= nil))))
      
;;
;;
;;
(deftest test-sparse-termlist
  (testing "test Term"
    (is (= '(Term 2 (SchemeNumber 2) (->list (->Term 2 (->SchemeNumber 2))))))
    (is (= [2 2] (->vec (->Term 2 (->SchemeNumber 2)))))
    )
  (testing "test adjoin"
      (is (= [[3 3] [2 2]] (->vec (->> the-empty-termlist
                                       (adjoin-term (make-num-term 2 2))
                                       (adjoin-term (make-num-term 3 3))))))
      (is (= [] (->vec (->> the-empty-termlist
                            (adjoin-term (make-num-term 1 0))))))
    )
  (testing "test make-sparse-num-termlist"
      (is (= [[2 2] [3 3]] (->vec (make-sparse-num-termlist [[2 2] [3 3]]))))
      (is (= [[0 2] [3 3]] (->vec (make-sparse-num-termlist [[0 2] [3 3]]))))
    )
  (testing "test empty-term? first-term rest-terms"
    (is (empty-term? the-empty-termlist))
    (is (empty-term? (rest-terms (make-sparse-num-termlist [[1 2]]))))
    (is (= [1 2] (->vec (first-term (make-sparse-num-termlist [[1 2] [2 3]])))))
    (is (= [[2 3]] (->vec (rest-terms (make-sparse-num-termlist [[1 2] [2 3]])))))
    )
  (testing "test add-terms"
    (is (= [[1 6]] (->vec (add-terms (make-sparse-num-termlist [[1 2]])
                                     (make-sparse-num-termlist [[1 4]])))))
    (is (= [[0 6]] (->vec (add-terms (make-sparse-num-termlist [[0 2]])
                                         (make-sparse-num-termlist [[0 4]])))))
    (is (= [[2 2] [1 5]] (->vec (add-terms (make-sparse-num-termlist [[2 2] [1 1]])
                                               (make-sparse-num-termlist [[1 4]])))))
    (is (= [[2 3] [1 6] [0 1]] (->vec (add-terms (make-sparse-num-termlist [[2 3] [1 2]])
                                                     (make-sparse-num-termlist [[1 4] [0 1]])))))
    )
  (testing "test add-sparse-num-terms"
    (is (= [[2 3] [1 6] [0 1]] (->vec (add-sparse-num-terms [[2 3] [1 2]]
                                                            [[1 4] [0 1]]))))
    )
  (testing "test mul-term-by-all-terms"
    (is (= [[2 1]] (->vec (mul-term-by-all-terms (make-num-term 1 1)
                                                 (make-sparse-num-termlist [[1 1]])))))
    (is (= [[2 1] [1 1]] (->vec (mul-term-by-all-terms (make-num-term 1 1)
                                                       (make-sparse-num-termlist [[1 1] [0 1]])))))
    )
  (testing "test mul-terms"
    (is (= [[2 1] [1 1]] (->vec (mul-terms (make-sparse-num-termlist [[1 1]])
                                           (make-sparse-num-termlist [[1 1] [0 1]])))))
    )
  (testing "test mul-num-terms"
    (is (= [[2 1] [1 1]] (->vec (mul-num-terms [[1 1]]
                                               [[1 1] [0 1]]))))
    )
 ) 

