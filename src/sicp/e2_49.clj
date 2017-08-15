(ns sicp.e2_49
  (:import [javax.swing JFrame JPanel]
           [java.awt Color])
  (:require [sicp.frame-painter :refer :all]))

;;
;; Exercise 2.49 (a)
;;
(defn outline-painter []
  (segment->painter (list 
                      (->Segment (->Vector 0 0) (->Vector 0 1))
                      (->Segment (->Vector 0 0) (->Vector 1 0))
                      (->Segment (->Vector 1 0) (->Vector 1 1))
                      (->Segment (->Vector 0 1) (->Vector 1 1)))))
;;
;; Exercise 2.49 (b)
;;
(defn corner-x-painter []
  (segment->painter (list 
                      (->Segment (->Vector 0 0) (->Vector 1 1))
                      (->Segment (->Vector 0 1) (->Vector 1 0)))))

;;
;; Exercise 2.49 (c)
;;
(defn diamond-painter []
  (segment->painter (list 
                      (->Segment (->Vector 0.5 0) (->Vector 1 0.5))
                      (->Segment (->Vector 1 0.5) (->Vector 0.5 1))
                      (->Segment (->Vector 0.5 1) (->Vector 0 0.5))
                      (->Segment (->Vector 0 0.5) (->Vector 0.5 0)))))

(defn main-painter [g]
  (let [f (->Frame g (->Vector 0 0) (->Vector 300 0) (->Vector 0 300))]
    ((corner-x-painter) f)
    ((outline-painter) f)
    ((diamond-painter) f)))

(defn e2_49-main []
  (let [frame (JFrame. "Test Draw")]
    (doto frame
      (.add (proxy [JPanel] []
              (paintComponent [g] (main-painter g))))
      (.pack)
      (.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize WIDTH HEIGHT)
      (.setVisible true))))

