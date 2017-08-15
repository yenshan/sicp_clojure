(ns sicp.e2_46)

(defrecord Vector [x y])

(defmulti add (fn [e1 e2] [(class e1) (class e2)]))
(defmulti sub (fn [e1 e2] [(class e1) (class e2)]))
(defmulti scale (fn [e1 e2] [(class e1) (class e2)]))

(defmethod add [Vector Vector] [v1 v2]
  (->Vector (+ (:x v1) (:x v2))
            (+ (:y v1) (:y v2))))

(defmethod sub [Vector Vector] [v1 v2]
  (->Vector (- (:x v1) (:x v2))
            (- (:y v1) (:y v2))))

(defmethod scale [Number Vector] [s v]
  (->Vector (* s (:x v))
            (* s (:y v)))) 

(add (->Vector 1 2) (->Vector 3 4))
(sub (->Vector 5 4) (->Vector 3 4))
(scale 10 (->Vector 3 4))

