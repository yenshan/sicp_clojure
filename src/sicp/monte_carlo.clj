(ns sicp.monte-carlo
  (:require [clojure.test :refer :all]
            [clojure.algo.generic.math-functions :as math]))

(defn remainder [a b]
  (if (< a b)
    a
    (recur (- a b) b)))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (remainder a b))))

(def random-init 100)

(defn random-update [x]
  (mod (+ (* 123 x) 353) 277))

(def -rand 
  (let [x (atom random-init)]
    (fn []
      (swap! x random-update)
      @x)))

(defn cesaro-test []
  (= (gcd (-rand) (-rand)) 1))

(defn monte-carlo [trials experiment]
  (letfn [(iter [trials-remaining trials-passed]
            (cond (= trials-remaining 0)
                   (/ trials-passed trials)
                  (experiment) 
                   (recur (dec trials-remaining) (inc trials-passed))
                  :else 
                   (recur (dec trials-remaining) trials-passed)))]
    (iter trials 0)))

(defn estimate-pi [trials]
  (math/sqrt (/ 6 (monte-carlo trials cesaro-test))))

;;
;;
;;
(defn random-gcd-test [trials initial-x]
  (letfn [(iter [trials-remaining trials-passed x]
            (let [x1 (random-update x)
                  x2 (random-update x1)]
             (cond (= trials-remaining 0)
                    (/ trials-passed trials)
                   (= (gcd x1 x2) 1)
                    (recur (dec trials-remaining) (inc trials-passed) x2)
                   :else 
                    (recur (dec trials-remaining) trials-passed x2))))]
    (iter trials 0 initial-x)))

(defn estimate-pi2 [trials]
  (math/sqrt (/ 6 (random-gcd-test trials random-init))))

