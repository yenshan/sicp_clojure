(ns sicp.complex-number
  (:require [clojure.test :refer :all])
  (:require [clojure.algo.generic.math-functions :refer [atan cos sin]]))

(defn square [x] (* x x))

(defn attach-tag [type-tag contents]
  (list type-tag contents))

(defn type-tag [datum]
  (if (coll? datum)
    (first datum)
    (println "Bad tagged datum -- TYPE-TAG" datum)))

(defn contents [datum]
  (if (coll? datum)
    (second datum)
    (println "Bad tagged datum -- CONTENTS" datum)))

(def table-operations (atom {}))

(defn tput [key1 key2 func]
  (swap! table-operations assoc key1 {key2 func}))

(defn tget [key1 key2]
  (-> @table-operations
      (get key1)
      (get key2)))

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [x y] (list x y))
          (magnitude [z]
            (Math/sqrt (+ (square (real-part z))
                          (square (imag-part z)))))
          (angle [z]
            (atan (imag-part z) (real-part z)))
          (make-from-mag-ang [r a]
            (list (* r (cos a)) (* r (sin a))))

          ;;
          (tag [x] (attach-tag 'rectangular x))]

    (tput 'real-part '(rectangular) real-part)
    (tput 'imag-part '(rectangular) imag-part)
    (tput 'magnitude '(rectangular) magnitude)
    (tput 'angle '(rectangular) angle)
    (tput 'make-from-real-imag 'rectangular
      (fn [x y] (tag (make-from-real-imag x y))))
    (tput 'make-from-mag-ang 'rectangular
      (fn [r a] (tag (make-from-mag-ang r a))))
    ))



;(defn add-complex [z1 z2]
;  (make-from-real-imag (+ (real-part z1) (real-part z2))
;                       (+ (imag-part z1) (imag-part z2))))
;
;(defn sub-complex [z1 z2]
;  (make-from-real-imag (- (real-part z1) (real-part z2))
;                       (- (imag-part z1) (imag-part z2))))
;
;(defn mul-complex [z1 z2]
;  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
;                     (* (angle z1) (angle z2))))
;
;(defn div-complex [z1 z2]
;  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
;                     (- (angle z1) (angle z2))))

(deftest test-complex-number
  (testing "tput, tget"
    (let [_ (tput 'a 'b 1)]
      (is (= 1 (tget 'a 'b))) 
    ))
  (testing "install-rectangular-package"
    (let [_ (install-rectangular-package)
          real-part (tget 'real-part '(rectangular))
          imag-part (tget 'imag-part '(rectangular))
          make-from-real-imag (tget 'make-from-real-imag 'rectangular)
          ]
      (is (= 'a (real-part '(a b))))
      (is (= 'b (imag-part '(a b))))
      (is (= '(rectangular (a b)) (make-from-real-imag 'a 'b)))
      )))
