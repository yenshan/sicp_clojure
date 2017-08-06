(ns sicp.e2_11)

(defrecord Interval [lower upper])

(defn add [x y]
  (->Interval (+ (:lower x) (:lower y))
              (+ (:upper x) (:upper y))))

(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))

(defn mul [x y]
  (let [lx (:lower x)
        ux (:upper x)
        ly (:lower y)
        uy (:upper y)]
    (cond (and (positive? lx) (positive? ux))
          (cond (and (positive? ly) (positive? uy)) (->Interval (* lx ly) (* ux uy))
                (and (negative? ly) (negative? uy)) (->Interval (* ux ly) (* lx uy))
                (and (negative? ly) (positive? uy)) (->Interval (* ux ly) (* ux uy)))
          (and (negative? lx) (negative? ux))
          (cond (and (positive? ly) (positive? uy)) (->Interval (* lx uy) (* ux ly))
                (and (negative? ly) (negative? uy)) (->Interval (* ux uy) (* lx ly))
                (and (negative? ly) (positive? uy)) (->Interval (* lx uy) (* lx ly)))
          (and (negative? lx) (positive? ux))
          (cond (and (positive? ly) (positive? uy)) (->Interval (* lx uy) (* ux uy))
                (and (negative? ly) (negative? uy)) (->Interval (* ux ly) (* lx uy))
                (and (negative? ly) (positive? uy)) (->Interval (min (* lx uy) (* ux ly))
                                                                (max (* lx ly) (* ux uy)))))))

(defn div [x y]
  (if (and (<= (:lower y) 0)
           (>= (:upper y) 0))
    (println "error: interval spannig zero.")
    (mul x
         (->Interval (/ 1.0 (:upper y))
                     (/ 1.0 (:lower y))))))


(mul (->Interval 1 2) (->Interval 3 4))
(mul (->Interval 1 2) (->Interval -4 -3))
(mul (->Interval 1 2) (->Interval -4 3))
(mul (->Interval -2 -1) (->Interval 3 4))
(mul (->Interval -2 -1) (->Interval -4 -3))
(mul (->Interval -2 -1) (->Interval -3 4))
(mul (->Interval -1 2) (->Interval 3 4))
(mul (->Interval -1 2) (->Interval -4 -3))
(mul (->Interval -1 2) (->Interval -3 4))

