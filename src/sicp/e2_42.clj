(ns sicp.e2_42
  (:require [sicp.nested-mapping :refer [flatmap]]))

;;
;; data structure for position of queen
;;
(defrecord Position [col row])

(defmulti -print (fn [entity] (class entity)))

(defmethod -print Position [p]
  (printf "(%d,%d)" (:col p) (:row p)))

(defn print-queens [colls]
  (doseq [col colls]
    (doseq [item col]
      (-print item))
    (print "\n")))

;;
;; functions for checking position
;;
(defn seq-pos [k {:keys [col row]}]
  (+ col (* k (- row 1))))

(defn same-row? [a b]
  (= (:row a) (:row b)))

(defn left-bottom? [k a b]
  (let [pa (seq-pos k a)
        pb (seq-pos k b)
        c (- pb pa)]
    (not (= nil (some #(= c (* (- k 1) %))
                      (range 1 k))))))

(defn left-top? [k a b]
  (let [pa (seq-pos k a)
        pb (seq-pos k b)
        c (- pa pb)]
    (not (= nil (some #(= c (* (+ k 1) %))
                      (range 1 k))))))

;;
;; definition of functions described in exercise 2.42
;;
(def empty-board '())

(defn safe? [k positions]
  (let [nq (first positions)]
    (= nil 
       (some #(or (same-row? nq %)
                  (left-top? k nq %)
                  (left-bottom? k nq %))
             (rest positions)))))

(defn adjoin-position [row col coll]
  (cons (->Position col row) coll))

(defn queens [board-size]
  (letfn [(queen-cols [k]
            (if (zero? k)
              (list empty-board)
              (filter
                (fn [positions] (safe? k positions))
                (flatmap 
                  (fn [rest-of-queens]
                    (map (fn [new-row]
                     (adjoin-position new-row k rest-of-queens))
                   (range 1 (inc board-size))))
                  (queen-cols (dec k))))))]
    (queen-cols board-size)))

(print-queens (queens 4))


;;
;; implement function queens in Clojure style
;;
(defn clj-queens [board-size]
  (letfn [(queen-cols [k]
            (if (zero? k)
              (list empty-board)
              (for [rest-of-queens (queen-cols (dec k))
                    new-row (range 1 (inc board-size))
                    :let [elem (adjoin-position new-row k rest-of-queens)]
                    :when (safe? k elem)]
                elem)))]
    (queen-cols board-size)))

(print-queens (clj-queens 6))

(= (clj-queens 6) (queens 6))


