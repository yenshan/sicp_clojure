(ns sicp.factorial)

(defn fact [n]
  (if (= n 1)
    1
    (* n (fact (dec n)))))

(defn factorial-iter [n]
  (letfn [(iter [product counter]
            (if (> counter n)
              product
              (recur (* counter product) (inc counter))))]
    (iter 1N 1)))

(defn fact-iter
  ([n] (fact-iter 1 1 n))
  ([n c p] 
   (if (> c n)
     p
     (recur n (inc c) (* c p)))))


(defn test1 [& args]
  (println (factorial 10)))

