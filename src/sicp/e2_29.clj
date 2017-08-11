(ns sicp.e2_29)

(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

;;
;; exercise 2.29a
;;
(defn left-branch [x] (first x))
(defn right-branch [x] (last x))
(defn branch-length [x] (first x))
(defn branch-structure [x] (last x))

;;
;; exercise 2.29b
;;
(defn sum-weight [branch sum]
  (if-not (zero? (branch-length branch))
    (+ sum (branch-length branch))
    (total-weight (branch-structure branch) sum)))

(defn total-weight
  ([mobile] (total-weight mobile 0))
  ([mobile sum]
     (+ (sum-weight (left-branch mobile) sum)
       (sum-weight (right-branch mobile) sum))))

;;
;; exercise 2.29c
;;
(defn balanced? [mobile]
  (= (sum-weight (left-branch mobile) 0)
     (sum-weight (right-branch mobile) 0)))


;;
;; test
;;
(def bm (make-mobile (make-branch 50
                                  nil)
                     (make-branch 0 
                                  (make-mobile (make-branch 20 nil)
                                               (make-branch 30 nil)))))
(defn e2_29-test []
  (println (total-weight bm))
  (println (balanced? bm)))
