(ns sicp.e1_37
  (:require [sicp.fixed-point :refer [fixed-point]]))

(def golden-ratio 
  (fixed-point #(+ 1 (/ 1 %)) 1.0))

(def one-gold-ratio (/ 1 golden-ratio))

(defn close-enough? [x y]
  (< (Math/abs (- x y)) 0.00001))

;;
;; continued fraction by recursive function
;;
(defn cont-frac
  ([nf df k] (cont-frac nf df k 1))
  ([nf df k i]
   (if (> i k)
     0
     (let [n (nf i)
           d (+ (df i)
                (cont-frac nf df k (inc i)))]
       (/ n d)))))

;;
;; continued fraction by iterative function
;;
(defn cont-frac-iter
  ([nf df k] (cont-frac-iter nf df k 0))
  ([nf df k res]
   (if (= k 0)
     res
     (recur nf df (dec k) (/ (nf k) (+ (df k) res))))))

;;
;;
(defn get-k-to-golden-ratio
  ([f] (get-k-to-golden-ratio f 1))
  ([f i]
   (if (close-enough? one-gold-ratio (f i))
     (do (println "res =" (f i))
         i)
     (recur f (inc i)))))

(defn test1 []
  (println "k ="
           (get-k-to-golden-ratio #(cont-frac (fn [i] 1.0) (fn [i] 1.0) %))))

(defn test2 []
  (println "k ="
           (get-k-to-golden-ratio #(cont-frac-iter (fn [i] 1.0) (fn [i] 1.0) %))))
