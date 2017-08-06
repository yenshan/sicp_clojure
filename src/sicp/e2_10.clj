(ns sicp.e2_10)

(defn make-interval [a b] (list a b))
(defn lower-bound [x] (first x))
(defn upper-bound [x] (last x))

(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound y) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (if (or (zero? (lower-bound y))
          (zero? (upper-bound y)))
    (println "error: to divide by zero.")
    (mul-interval
      x
      (make-interval (/ 1.0 (lower-bound y))
                     (/ 1.0 (upper-bound y))))))


(def a (make-interval 10 12))
(def b (make-interval 13 15))
(def zero1 (make-interval 0 15))
(def zero2 (make-interval 10 0))

(add-interval a b)
(mul-interval a b)
(div-interval a zero1)
(div-interval a zero2)

