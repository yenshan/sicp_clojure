(ns sicp.e2_50
  (:import [javax.swing JFrame JPanel])
  (:require [sicp.frame-painter :refer :all]))


(defn flip-horiz [painter]
  (transform-painter painter
                     (->Vector 1.0 0.0)
                     (->Vector 0.0 0.0)
                     (->Vector 1.0 1.0)))

(defn rotate180 [painter]
  (transform-painter painter
                     (->Vector 1.0 1.0)
                     (->Vector 0.0 1.0)
                     (->Vector 1.0 0.0)))

(defn rotate270 [painter]
  (transform-painter painter
                     (->Vector 0.0 1.0)
                     (->Vector 1.0 1.0)
                     (->Vector 0.0 0.0)))

(defn paint [g f painter]
 ; ((flip-horiz painter) g f)
  ((rotate180 painter) g f)
  ;((rotate270 painter) g f)
  )

(defn e2_50-main []
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
