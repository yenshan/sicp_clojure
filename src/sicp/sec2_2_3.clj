(ns sicp.sec2-2-3)

(defn square [x] (* x x))

(defn sum-odd-squares [tree]
  (cond (and (coll? tree) (empty? tree)) 0
        (not (coll? tree)) (if (odd? tree)
                             (square tree)
                             0)
        :else (+ (sum-odd-squares (first tree))
                 (sum-odd-squares (rest tree)))))

(def ttree (list 1 (list 2 3) (list 5 (list 6 7))))

(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn even-fibs [n]
  (letfn [(next [k]
            (if (> k n)
              nil
              (let [f (fib k)]
                (if (even? f)
                  (cons f (next (+ k 1)))
                  (next (+ k 1))))))]
    (next 0)))

(defn even-fibs'
  ([n] (even-fibs' n 0 []))
  ([n k res]
   (if (> k n)
     res
     (let [f (fib k)
           res' (if (even? f) (conj res f) res)]
         (recur n (inc k) res')))))

(defn sec2-2-3-test []
  (println (= (sum-odd-squares ttree)
              (reduce + (map square (filter odd? [1 2 3 4 5 6 7]))))))

