(ns sicp.factorial
  (:gen-class))

(defn factorial [n]
  (if (= n 1)
    1
    (* n (factorial (dec n)))))

(defn factorial-iter [n]
  (letfn [(iter [product counter]
            (if (> counter n)
              product
              (recur (* counter product) (inc counter))))]
    (iter 1N 1)))

(defn factorial-iter'
  ([n] (factorial-iter' 1N 1 n))
  ([product counter max-count] 
   (if (> counter max-count)
     product
     (recur (* counter product) (inc counter) max-count))))

