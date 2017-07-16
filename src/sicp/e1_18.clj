(ns sicp.e1_18
  (gen-class))

(defn -double [a] (* a 2))
(defn -halve [a] (/ a 2))

(defn mult-iter
  ([a b] (mult-iter a b 0))
  ([base cnt sum]
   (cond
     (zero? cnt) sum
     (even? cnt) (recur (-double base) (-halve cnt) sum)
     :else (recur base (dec cnt) (+ sum base)))))
