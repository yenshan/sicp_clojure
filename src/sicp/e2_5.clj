(ns sicp.e2_5)

(defn expt
  ([base n] (expt base n 1))
  ([base n res]
   (if (zero? n)
     res
     (recur base (dec n) (* base res)))))

(defn log
  ([base x] (log base x 0))
  ([base x n] (if (< x base)
           n
           (recur base (/ x base) (inc n)))))

(defn divide-mod [x base]
  (if-not (zero? (mod x base))
    x
    (recur (/ x base) base)))

;
; define cons, car, cdr only using arithmetic computing
; 2^a * 3^b
;
(defn cons- [a b]
  (* (expt 2 a) (expt 3 b)))

(defn car [x]
  (log 2 (divide-mod x 3)))

(defn cdr [x]
  (log 3 (divide-mod x 2)))

;
;
;
(defn e2_5-test []
  (let [a (cons- 6 9)]
    (println (car a))
    (println (cdr a))))
