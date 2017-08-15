(ns sicp.the-picture-language)

(defn beside [left right])
(defn below [bottom top])

(defn flip-horiz [painter])
(defn flip-vert [painter])

;;
;;
;;
(defn flipped-pairs [painter]
  (let [painter2 (beside painter (flip-vert painter))]
    (below painter2 painter2)))

(defn up-split [painter n]
  (if (zero? 0)
    painter
    (let [smaller (up-split painter (dec n))]
      (below painter (beside smaller smaller)))))

(defn right-split [painter n]
  (if (zero? 0)
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

(def rotate180 (comp flip-vert flip-horiz))

(defn square-limit2 [painter n]
  (let [combine4 (square-of-four flip-vert identity
                                 rotate180 flip-vert)]
    (combine4 (corner-split painter n))))

;;
;; Frames
;;

(defn origin-frame [frame])
(defn add-vect [a b])
(defn scale-vect [a b])
(defn xcor-vect [v])
(defn ycor-vect [v])
(defn edge1-frame [frame])
(defn edge2-frame [frame])

(defn frame-coord-map [frame]
  (fn [v]
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))
              
