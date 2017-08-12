(ns sicp.e2_39
  (:require [sicp.e2_38 :refer [fold-right fold-left]]))

(defn reverse1 [seqc]
  (fold-right (fn [x y] (concat y (list x))) nil seqc))

(defn reverse2 [seqc]
  (fold-left (fn [x y] (cons y x)) nil seqc))

(def tlist '(1 2 3 4 5))

(reverse1 tlist)
(reverse2 tlist)
