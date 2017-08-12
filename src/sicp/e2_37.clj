(ns sicp.e2_37
  (:require [sicp.sec2-2-3 :refer [accumulate]])
  (:require [sicp.e2_36 :refer [accumulate-n]]))

(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

;;
;; dot-product
;;
(def seq1 '(1 2 3 4))
(def seq2 '((1 2 3 4) (4 5 6 7) (6 7 8 9)))

(dot-product seq1 (first seq2))

;;
;; matrix-*-vector
;;
(defn matrix-*-vector [m v]
  (map #(dot-product %1 v) m))

(matrix-*-vector seq2 seq1)

;;
;; transpose
;;
(defn transpose [mat]
  (accumulate-n cons () mat))

(transpose seq2)

;;
;; matrix-*-matrix
;;
(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %1) m)))

(def mat1 '((1 2) (3 4)))
(def mat2 '((2 3) (4 5)))
(matrix-*-matrix mat1 mat2)
