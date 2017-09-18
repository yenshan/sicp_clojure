(ns sicp.e3-1
  (:require [clojure.test :refer :all]))

(defn make-accumulator [initial]
  (let [sum (atom initial)]
    (fn [n] (swap! sum #(+ % n)))))

(deftest e3-1-test
  (testing "make-accumulator"
    (let [A (make-accumulator 5)]
      (is (= 15 (A 10)))
      (is (= 25 (A 10)))
      )))
