(ns sicp.e2_2
  (gen-class))

(defn make-point [x y] (list x y))
(defn x-point [p] (first p))
(defn y-point [p] (last p))

(defn make-segment [start end] (list start end))
(defn start-segment [s] (first s))
(defn end-segment [s] (last s))

(defn midpoint-segment [s]
  (let [ss (start-segment s)
        es (end-segment s)]
    (make-point (/ (+ (x-point ss) (x-point es)) 2)
                (/ (+ (y-point ss) (y-point es)) 2))))

(defn point->str [p]
  (str "(" (x-point p) "," (y-point p) ")"))

(defn segment->str [s]
  (str (point->str (start-segment s))
       "--" 
       (point->str (end-segment s))))

(def seg (make-segment (make-point 10 20)
                       (make-point 20 30)))

(x-point (midpoint-segment seg)) ; -> 15
(y-point (midpoint-segment seg)) ; -> 25

(defn e2_2-test 
  [& args]
  (let [seg (make-segment (make-point 10 20)
                          (make-point 20 30))]
    (println "segment is " (segment->str seg))
    (println "midpoint is " (point->str (midpoint-segment seg)))))
