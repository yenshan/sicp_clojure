(ns sicp.e3-2
  (:require [clojure.test :refer :all]
            [clojure.algo.generic.math-functions :as math]))

(defn make-monitored [f]
  (let [counter (atom 0)]
    (fn [m]
      (if (= m 'how-many-calls?)
        @counter
        (do (swap! counter inc)
            (f m))))))
                          

(deftest e3-2-test
  (testing "make-monitored"
    (let [s (make-monitored math/sqrt)]
      (is (= 10.0 (s 100)))
      (is (= 1 (s 'how-many-calls?)))
      )))
