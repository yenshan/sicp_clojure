(ns sicp.e1_17
  (gen-class))

(defn mult [a b]
  (if (= b 0)
    0
    (+ a (mult a (dec b)))))

(defn -double [a] (* a 2))
(defn -halve [a] (/ a 2))

(defn fast-mult [a b]
  (cond
    (zero? b) 0
    (even? b) (fast-mult (-double a) (-halve b))
    :else (+ a (fast-mult a (dec b)))))
