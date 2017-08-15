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
(defrecord Frame [origin edge1 edge2])

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
  (fn [g frame]
    (doseq [segment segment-list]
      (draw-line g
                 ((frame-coord-map frame) (:start segment))
                 ((frame-coord-map frame) (:end segment))))))

(defn transform-painter [painter origin corner1 corner2]
  (fn [g frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
        g
        (->Frame new-origin
                 (sub (m corner1) new-origin)
                 (sub (m corner2) new-origin))))))

(defn flip-vert [painter]
  (transform-painter painter
                     (->Vector 0.0 1.0)
                     (->Vector 1.0 1.0)
                     (->Vector 0.0 0.0)))

(defn shurink-to-upper-right [painter]
  (transform-painter painter
                     (->Vector 0.5 0.5)
                     (->Vector 1.0 0.5)
                     (->Vector 0.5 1.0)))

(defn rotate90 [painter]
  (transform-painter painter
                     (->Vector 1.0 0.0)
                     (->Vector 1.0 1.0)
                     (->Vector 0.0 0.0)))

(defn squash-inwards [painter]
  (transform-painter painter
                     (->Vector 0.0 0.0)
                     (->Vector 0.65 0.35)
                     (->Vector 0.35 0.65)))

(defn beside [painter1 painter2]
  (let [split-point (->Vector 0.5 0.0)
        paint-left (transform-painter painter1
                                      (->Vector 0.0 0.0)
                                      split-point
                                      (->Vector 0.0 1.0))
        paint-right (transform-painter painter2
                                      split-point
                                      (->Vector 1.0 0.0)
                                      (->Vector 0.5 1.0))]
    (fn [g frame]
      (paint-left g frame)
      (paint-right g frame))))


;;
;; 
;;
(def pic-points [{:x 0.2 :y 0.8} {:x 0.8 :y 0.8}
                 {:x 0.2 :y 0.2} {:x 0.8 :y 0.2}
                 {:x 0.8 :y 0.8}])

(defn make-segment-list-from-points [coll]
  (map (fn [[p1 p2]]
         (->Segment (map->Vector p1)
                    (map->Vector p2)))
       (partition 2 1 coll)))

(defn- test-paint [g f painter]
  ((beside painter painter) g f)
;  ((squash-inwards painter) g f)
  ;((rotate90 painter) g f)
;  ((shurink-to-upper-right painter) g f)
; ((flip-vert painter) g f)
 )

(defn frame-painter-main []
  (letfn [(main-paint [g]
            (let [f (->Frame (->Vector 0 0) (->Vector 300 0) (->Vector 0 300))
                  segs (make-segment-list-from-points pic-points)
                  painter (segment->painter segs)]
              (test-paint g f painter)))]
    (let [frame (JFrame. "Test Draw")]
      (doto frame
        (.add (proxy [JPanel] []
                (paintComponent [g] (main-paint g))))
        (.pack)
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
        (.setSize WIDTH HEIGHT)
        (.setVisible true)))))

