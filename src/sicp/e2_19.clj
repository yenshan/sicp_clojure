(ns sicp.e2_19
  (gen-class))

(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(def no-more? empty?)
(def except-first-denomination rest)
(def first-denomination first)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else
        (+ (cc amount
               (except-first-denomination coin-values))
           (cc (- amount
                  (first-denomination coin-values))
               coin-values))))

(cc 100 us-coins)
