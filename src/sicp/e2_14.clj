(ns sicp.e2_14)

(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))

;;
;;
(defrecord Interval [lower upper])

(defn add [x y]
  (->Interval (+ (:lower x) (:lower y))
              (+ (:upper x) (:upper y))))

(defn sub [x y]
  (->Interval (- (:lower x) (:upper y))
              (- (:upper x) (:lower y))))

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
;;
;;
;;
(defn make-center-percent [center percent]
  (let [width (* center (/ percent 100.0))]
    (->Interval (- center width) (+ center width))))

(defn center [i]
  (/ (+ (:lower i) (:upper i)) 2))

(defn percent [i]
  (-> (- (:upper i) (:lower i))
      (/ 2)
      (/ (center i))
      (Math/abs)
      (* 100)))
;;
;;
;;
(defn par1 [r1 r2]
  (div (mul r1 r2) (add r1 r2)))

(defn par2 [r1 r2]
  (let [one (->Interval 1 1)]
    (div one
         (add (div one r1) (div one r2)))))

(def a (make-center-percent 10 10))
(def b (make-center-percent 10 5))

(center (div a a))
(center (div a b))

(center (par1 a b))
(center (par2 a b))
