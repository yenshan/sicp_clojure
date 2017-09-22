(ns sicp.e3-4
  (:require [clojure.test :refer :all]))

(defn call-the-cops []
  "cops")

(defn make-account [initial passwd]
  (let [balance (atom initial)
        incorrect-counter (atom 1)]
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
                (fn [a] (if (> @incorrect-counter 7)
                          (call-the-cops)
                          (do (swap! incorrect-counter inc)
                              "Incorrect password")))))]
      dispatch)))

(deftest e3-4-test
  (testing "make-account"
    (let [A (make-account 100 'my-pass)]
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "Incorrect password" ((A 'may-pass 'withdraw) 10)))
      (is (= "cops" ((A 'may-pass 'withdraw) 10)))
      )))

