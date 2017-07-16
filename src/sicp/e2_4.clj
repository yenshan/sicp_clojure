(ns sicp.e2_4
  (gen-class))

(defn mycons [x y]
  (fn [m] (m x y)))

(defn car [z]
  (z (fn [a b] a)))

(defn cdr [z]
  (z (fn [a b] b)))
