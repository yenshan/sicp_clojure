(ns sicp.sec2-2-3)

;;
;; 2.2.3 Squences as Convenetional Interfaces
;;
(defn square [x] (* x x))

(defn sum-odd-squares [tree]
  (cond (and (coll? tree) (empty? tree)) 0
        (not (coll? tree)) (if (odd? tree)
                             (square tree)
                             0)
        :else (+ (sum-odd-squares (first tree))
                 (sum-odd-squares (rest tree)))))

(defn fib [n]
  (cond (zero? n) 0
        (= n 1) 1
        :else (+ (fib (- n 1))
                 (fib (- n 2)))))

(defn sicp-even-fibs [n]
  (letfn [(next [k]
            (if (> k n)
              nil
              (let [f (fib k)]
                (if (even? f)
                  (cons f (next (+ k 1)))
                  (next (+ k 1))))))]
    (next 0)))

(defn even-fibs
  ([n] (even-fibs n 0 []))
  ([n k res]
   (if (> k n)
     res
     (let [f (fib k)
           res' (if (even? f) (conj res f) res)]
         (recur n (inc k) res')))))


(defn sec2-2-3-test []
  (let [ttree (list 1 (list 2 3) (list 5 (list 6 7)))]
    (println (= (sum-odd-squares ttree)
                (reduce + (map square (filter odd? [1 2 3 4 5 6 7])))))
    (println (= (sicp-even-fibs 10)
                (even-fibs 10)))))

;;
;; Sequences Operations
;;
(defn -filter [pred seqc]
  (cond (and (coll? seqc) (empty? seqc)) nil
        (pred (first seqc)) 
          (cons (first seqc)
              (-filter pred (rest seqc))) 
        :else (-filter pred (rest seqc))))

(-filter odd? (list 1 2 3 4 5))
;-> (1 3 5)

(defn accumulate [op initial seqc]
  (if (and (coll? seqc) (empty? seqc))
    initial
    (op (first seqc)
        (accumulate op initial (rest seqc)))))

(accumulate + 0 (list 1 2 3 4 5))
;-> 15
(accumulate cons nil (list 1 2 3 4 5))
;-> (1 2 3 4 5)

(defn enumerate-interval [low high]
  (if (> low high)
    nil
    (cons low (enumerate-interval (inc low) high))))

(enumerate-interval 2 7)
;-> (2 3 4 5 6 7)

(defn enumerate-tree [tree]
  (cond (and (coll? tree) (empty? tree)) nil
        (not (coll? tree)) (list tree)
        :else (concat (enumerate-tree (first tree))
                      (enumerate-tree (rest tree)))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
;-> (1 2 3 4 5)

(defn sum-odd-sequare [tree]
  (accumulate + 0 
              (map square (-filter odd? (enumerate-tree tree)))))

(sum-odd-sequare (list 1 (list 2 (list 3 4)) 5))
;-> 35


(defn even-fibs [n]
  (accumulate cons 
              nil 
              (filter even? (map fib (enumerate-interval 0 n))))) 

(even-fibs 10)                      
;-> (0 2 8 34)

(defn list-fib-square [n]
  (accumulate cons
              nil
              (map square (map fib (enumerate-interval 0 n)))))

(list-fib-square 10)

(defn product-of-squares-of-odd-elements [seqc]
  (accumulate * 1 (map square (filter odd? seqc))))

(product-of-squares-of-odd-elements (list 1 2 3 4 5))


