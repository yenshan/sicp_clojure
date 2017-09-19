(ns sicp.e3-3
  (:require [clojure.test :refer :all]))

(defn make-account [initial passwd]
  (let [balance (atom initial)]
    (letfn [(withdraw [amount]
              (if (>= @balance amount)
                (reset! balance (- @balance amount))
                ("Insufficient funds")))
            (deposit [amount]
              (reset! balance (+ @balance amount)))
            (dispatch [p m]
              (if (= p passwd)
                (condp = m
                      'withdraw withdraw
                      'deposit deposit 
                      :else (assert "Unkown request -- MAKE ACCOUNT" m))
                (fn [a] "Incorrect password")))]
      dispatch)))

(deftest e3-3-test
  (testing "make-acount"
    (let [acc (make-account 100 'secret-password)]
      (is (= 60 ((acc 'secret-password 'withdraw) 40)))
      (is (= "Incorrect password" ((acc 'some-other-password 'withdraw) 40)))
      )
    ))
