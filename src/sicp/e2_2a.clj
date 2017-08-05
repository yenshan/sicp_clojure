(ns sicp.e2_2a)

;;
;; Do exercise 2.2 using Clojure's record
;;

(defrecord Point [x y])
(defrecord Segment [start end])

(defn midpoint-segment 
  [{:keys [start end]}]
    (->Point (/ (+ (:x start) (:x end)) 2)
             (/ (+ (:y start) (:y end)) 2)))

(defmulti ->str (fn [entity] (class entity)))

(defmethod ->str Point [p]
  (str "(" (:x p) "," (:y p) ")"))

(defmethod ->str Segment [s]
  (str (->str (:start s)) "--" (->str (:end s))))

(def seg (->Segment (->Point 10 20)
                    (->Point 20 30)))

(-> seg midpoint-segment :x)
(-> seg midpoint-segment :y)

(defn e2_2a-test
  [& args]
  (let [seg (->Segment (->Point 10 20)
                       (->Point 20 30))]
    (println "segment is " (->str seg))
    (println "midpoint is " (->str (midpoint-segment seg)))))
