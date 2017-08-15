(ns sicp.frame-painter
  (:import [javax.swing JFrame JPanel]
           [java.awt Color]))

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

;;
;; exercise 2.47: definition of frame
;;
(defrecord Frame [graphic origin edge1 edge2])

;;
;; exercise 2.48: definition of segment
;;
(defrecord Segment [start end])

;;
;;
;;
(defn frame-coord-map [frame]
  (fn [v]
    (add (:origin frame)
         (add (scale (:x v) (:edge1 frame))
              (scale (:y v) (:edge2 frame))))))

(def WIDTH 500)
(def HEIGHT 500)

(defn draw-line [g v1 v2]
  (doto g
    ;(println "draw-line" v1 v2)
    (.setColor Color/BLACK)
    (.drawLine (:x v1) (- HEIGHT (:y v1))
               (:x v2) (- HEIGHT (:y v2)))))

(defn segment->painter [segment-list]
  (fn [frame]
    (doseq [segment segment-list]
      (draw-line (:graphic frame)
                 ((frame-coord-map frame) (:start segment))
                 ((frame-coord-map frame) (:end segment))))))


(defn frame-painter-main []
  (letfn [(make-panel []
            (proxy [JPanel] []
              (paintComponent [g]
                (let [f (->Frame g (->Vector 0 0) (->Vector 300 0) (->Vector 0 300))
                      seg1 (->Segment (->Vector 0.4 0.1) (->Vector 0.7 0.5))
                      seg2 (->Segment (->Vector 0.3 0.3) (->Vector 0.4 0.4))]
                  ((segment->painter (list seg1 seg2)) f)))))]
    (let [frame (JFrame. "Test Draw")]
      (doto frame
        (.add (make-panel))
        (.pack)
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
        (.setSize WIDTH HEIGHT)
        (.setVisible true)))))

