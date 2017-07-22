(ns sicp.e1_43)

(defn apply-fn [f x n]
  (if (= n 1)
    (f x)
    (f (apply-fn f x (dec n)))))

(defn repeated [f n]
  (fn [x] (apply-fn f x n)))

(defn square [x] (* x x))

(defn e1_43-test []
  (println "((repeated square 2) 5) is"
           ((repeated square 2) 5)))

