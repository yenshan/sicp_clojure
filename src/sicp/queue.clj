(ns sicp.queue
  (:require [clojure.test :refer :all]))

(defn pair-cons [x y]
  (let [dx (atom x)
        dy (atom y)]
    (letfn [(set-x! [v]
              (reset! dx v))
            (set-y! [v]
              (reset! dy v))
            (dispatch [m]
              (condp = m
                'car @dx
                'cdr @dy
                'set-car! set-x!
                'set-cdr! set-y!
                :else (println "Undefined operation: CONS" m)))]
      dispatch)))

(defn car [z] (z 'car))

(defn cdr [z] (z 'cdr))

(defn set-car! [z new-value]
  ((z 'set-car!) new-value) z)
               
(defn set-cdr! [z new-value]
  ((z 'set-cdr!) new-value) z)

;; -----------------------------------
;; -----------------------------------
(defn front-ptr [queue] (car queue))

(defn rear-ptr [queue] (cdr queue))

(defn set-front-ptr! [queue item]
  (set-car! queue item))
  
(defn set-rear-ptr! [queue item]
  (set-cdr! queue item))

(defn empty-queue? [queue]
  (nil? (front-ptr queue)))

(defn make-queue [] (pair-cons nil nil))

(defn front-queue [queue]
  (if (empty-queue? queue)
    (println "FRONT called with empty queue." queue)
    (car (front-ptr queue))))

(defn insert-queue! [queue item]
  (let [new-pair (pair-cons item nil)]
    (if (empty-queue? queue)
      (do (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
      (do (set-cdr! (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue))))



(deftest test-queue
  (testing "cons, car, cdr"
    (is (= 1 (car (pair-cons 1 2))))
    (is (= 2 (cdr (pair-cons 1 2))))
    )
  (testing "queue"
    (let [q (insert-queue! (make-queue) 1)]
      (is (not (empty-queue? q)))
      (is (= 1 (front-queue q)))
      (is (= 1 (front-queue (insert-queue! q 2))))
      ))
  )

