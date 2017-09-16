(ns sicp.e2_90.dense-termlist
  (:require [clojure.test :refer :all])
  (:require [sicp.clj-generic-arithmetie-operations :refer :all])
  (:require [sicp.e2_90.termlist-interface :refer :all]))

;;
;; abstract data: term list
;;
(defrecord DenseTermList [dat])

(defmethod constructor DenseTermList [L]
  (fn [dat] (->DenseTermList dat)))

(defn make-dense-num-termlist [coll]
  (->DenseTermList 
    (map #(->SchemeNumber %) (reverse coll))))

(defmethod ->vec DenseTermList [coll]
  (vec (reverse 
         (map :dat (:dat coll)))))

(def ^:private the-empty-termlist (->DenseTermList []))

(defn- adjoin-term [term term-list]
  (->DenseTermList (cons term (:dat term-list))))


(defmethod add-terms [DenseTermList DenseTermList] [L1 L2]
  (cond (empty-term? L1) L2
        (empty-term? L2) L1
        :else (let [t1 (first-term L1)
                    t2 (first-term L2)]
                (adjoin-term (add t1 t2)
                             (add-terms (rest-terms L1)
                                        (rest-terms L2))))))

(defn- mul-term-by-all-terms [t1 L]
  (if (empty-term? L)
    the-empty-termlist
    (adjoin-term (mul t1 (first-term L)) 
                 (mul-term-by-all-terms t1 (rest-terms L)))))

(defmethod mul-terms [DenseTermList DenseTermList] [L1 L2]
  (letfn [(-mul [L1 L2 res]
            (if (empty-term? L1)
              res
              (recur (rest-terms L1) 
                     (adjoin-term (->SchemeNumber 0) L2) 
                     (cons (mul-term-by-all-terms (first-term L1) L2) res))))]
  (reduce add-terms the-empty-termlist (-mul L1 L2 '()))))


(defmethod =zero? DenseTermList [L]
  (= nil (some #(not (=zero? %)) (:dat L))))
      
;;
;;
;;
(deftest test-dense-termlist
  (testing "test ->vec"
    (is (= [] (->vec the-empty-termlist)))
    )
  (testing "test add-terms"
    (is (= [3 5] (->vec (add-terms (make-dense-num-termlist [1 2])
                                   (make-dense-num-termlist [2 3])))))
    (is (= [1 3 5] (->vec (add-terms (make-dense-num-termlist [1 2])
                                     (make-dense-num-termlist [1 2 3])))))
    )
  (testing "test mul-term-by-all-terms"
    (is (= [4 6] (->vec (mul-term-by-all-terms (->SchemeNumber 2)
                                               (make-dense-num-termlist [2 3]))))) 
    )
  (testing "test mul-terms"
    (is (= [2 7 6] (->vec (mul-terms (make-dense-num-termlist [1 2])
                                     (make-dense-num-termlist [2 3])))))
    (is (= [2 7 12 9] (->vec (mul-terms (make-dense-num-termlist [1 2 3])
                                        (make-dense-num-termlist [2 3])))))
    )
  (testing "test =zero?"
    (is (=zero? (make-dense-num-termlist [0 0 0])))
    (is (not (=zero? (make-dense-num-termlist [1 0 0]))))
    )
 ) 

