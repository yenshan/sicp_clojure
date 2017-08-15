(ns sicp.e2_51
  (:import [javax.swing JFrame JPanel])
  (:require [sicp.frame-painter :refer :all]))

;;
;; definition of below which is analogue to the beside
;;
(defn below [painter1 painter2]
  (let [split-point (->Vector 0.0 0.5)
        paint-bottom (transform-painter painter1
                                        (->Vector 0.0 0.0)
                                        (->Vector 1.0 0.0)
                                        split-point)
        paint-top (transform-painter painter2
                                     split-point
                                     (->Vector 1.0 0.5)
                                     (->Vector 0.0 1.0))]
    (fn [g frame]
      (paint-bottom g frame)
      (paint-top g frame))))

;;
;; definition of below by using rotate and beside
;;
(defn below2 [painter1 painter2]
  (let [painter1' (rotate270 painter1)
        painter2' (rotate270 painter2)]
    (rotate90 (beside painter1' painter2'))))


(defn paint [g f painter]
  ((below2 painter painter) g f)
  )

(defn e2_51-main []
  (letfn [(main-paint [g]
            (let [f (->Frame (->Vector 0 0) (->Vector 300 0) (->Vector 0 300))
                  segs (make-segment-list-from-points pic-points)
                  painter (segment->painter segs)]
              (paint g f painter)))]
    (let [frame (JFrame. "Test Draw")]
      (doto frame
        (.add (proxy [JPanel] []
                (paintComponent [g] (main-paint g))))
        (.pack)
        (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
        (.setSize WIDTH HEIGHT)
        (.setVisible true)))))
