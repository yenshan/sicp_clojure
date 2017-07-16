(ns sicp.gcd
  (gen-class))

(defn remainder [a b]
  (if (< a b)
    a
    (remainder (- a b) b)))

(defn gcd [a b]
  (if (zero? b)
    a
    (gcd b (remainder a b))))
