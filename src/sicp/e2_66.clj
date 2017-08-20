(ns sicp.e2_66
  (:require [clojure.test :refer :all]))

(defrecord Record [key data])

(defrecord Tree [entry left-branch right-branch])


(defn =key? [rec1 rec2] (= (:key rec1) (:key rec2)))
(defn <key? [rec1 rec2] (< (:key rec1) (:key rec2)))
(defn >key? [rec1 rec2] (> (:key rec1) (:key rec2)))

(defn adjoin-set [x set]
  (cond (empty? set) (->Tree x '() '())
        (=key? x (:entry set)) set
        (<key? x (:entry set)) (->Tree (:entry set)
                                       (adjoin-set x (:left-branch set))
                                       (:right-branch set))
        (>key? x (:entry set)) (->Tree (:entry set)
                                       (:left-branch set)
                                       (adjoin-set x (:right-branch set)))))

(def key-entry (comp :key :entry))

(defn lookup [given-key set-of-records]
  (cond (empty? set-of-records) false
        (= given-key (key-entry set-of-records)) (:entry set-of-records)
        (< given-key (key-entry set-of-records)) 
          (lookup given-key (:left-branch set-of-records))
        (> given-key (key-entry set-of-records)) 
          (lookup given-key (:right-branch set-of-records))))

(defn make-records [name-list]
  (reduce #(adjoin-set %2 %1) 
          '()
          (for [i name-list]
            (->Record (count i) i))))

(deftest test-e2_66
  (testing "test"
    (let [names ["Keyn" "Mie" "Maradona" "Cocoro" "An"]
          records (make-records names)]
      (is (= "Mie" (:data (lookup 3 records))))
      (is (= "An" (:data (lookup 2 records))))
     ))) 

