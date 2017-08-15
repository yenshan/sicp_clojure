(ns sicp.the-picture-language
  (:import [javax.swing JFrame JPanel])
  (:require [sicp.frame-painter :refer :all]))

;;
;;
;;
(defn flipped-pairs [painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn up-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

(defn right-split [painter n]
  (if (zero? n)
    painter
    (let [smaller (right-split painter (dec n))]
      (beside painter (below smaller smaller)))))

(defn corner-split [painter n]
  (if (zero? n)
    painter
    (let [up (up-split painter (dec n))
          right (right-split painter (dec n))
          top-left (beside up up)
          bottom-right (below right right)
          corner (corner-split painter (dec n))]
      (beside (below painter top-left)
              (below bottom-right corner)))))

(defn square-limit [painter n]
 (let [quarter (corner-split painter n)
       half (beside (flip-horiz quarter) quarter)]
   (below (flip-vert half) half)))

;;
;; higher-order operations
;;
(defn square-of-four [tl tr bl br]
  (fn [painter]
    (let [top (beside (tl painter) (tr painter))
          bottom (beside (bl painter) (br painter))]
      (below bottom top))))

(defn flipped-pairs2 [painter]
  (let [combine4 (square-of-four identity flip-vert
                                 identity flip-vert)]
    (combine4 painter)))

(defn square-limit2 [painter n]
  (let [combine4 (square-of-four flip-horiz identity
                                 rotate180 flip-vert)]
    (combine4 (corner-split painter n))))


;;
;; test paint
;;
(defn- test-paint [g f painter]
  ;((flipped-pairs painter) g f)
  ;((up-split painter 3) g f)
  ;((right-split painter 3) g f)
  ;((corner-split painter 3) g f)
  ;((square-limit painter 3) g f)
  ;((flipped-pairs2 painter) g f)
  ((square-limit2 painter 3) g f)
  )

(defn the-picture-language-main []
  (letfn [(main-paint [g]
            (let [f (->Frame (->Vector 0 0) (->Vector 400 0) (->Vector 0 400))
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

