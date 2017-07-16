(ns sicp.find-divisor
  (gen-class))

(defn divides? [a b] (= (rem b a) 0))
(defn square [a] (* a a))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (inc test-divisor))))

(defn smallest-divisor [n] (find-divisor n 2))

(defn prime? [n] (= n (smallest-divisor n)))
