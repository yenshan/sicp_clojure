(ns sicp.list-operations)

(def squares (list 1 4 9 16 25))

;;
;; list-ref
;;
(defn list-ref [items n]
  (if (zero? n)
    (first items)
    (list-ref (rest items) (dec n))))

(list-ref squares 3)
;-> 16

;;
;; length
;;
(defn length [items]
  (if (empty? items)
    0
    (+ 1 (length (rest items)))))

(length squares)
;-> 5

;;
;; length function by using iterative process
;;
(defn length' [items]
  (letfn [(length-iter [coll len]
            (if (empty? coll)
              len
              (recur (rest coll) (inc len))))]
    (length-iter items 0)))

(length' squares)

;;
;; lengh function by using multi-arity & iterative process
;;
(defn length''
  ([items] (length'' items 0))
  ([items cnt]
   (if (empty? items)
     cnt
     (recur (rest items) (inc cnt)))))

(length'' squares)
   

;;
;; append
;;
(defn append [list1 list2]
  (if (empty? list1)
    list2 
    (cons (first list1) (append (rest list1) list2))))

(def lst1 '(1 2 3))
(def lst2 '(3 4 5))
(append lst1 lst2)

