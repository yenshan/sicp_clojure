(ns sicp.fibonacci
  (:gen-class))

(defn fib-iter
  ([n] (fib-iter 1N 0N n))
  ([a b cnt]
   (if (zero? cnt)
     b
     (recur (+ a b) a (dec cnt)))))

(defn fib
  [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))
