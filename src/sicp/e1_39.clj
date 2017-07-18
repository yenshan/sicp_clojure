(ns sicp.e1_39)

(defn cont-frac
  ([nf df k] (cont-frac nf df k 1))
  ([nf df k i]
   (if (> i k)
     0
     (let [n (nf i)
           d (- (df i)
                (cont-frac nf df k (inc i)))]
       (/ n d)))))

;;
;; Lambert's continued fraction for tan x
;;
(defn tan-cf [x k]
  (cont-frac (fn [i] (if (= i 1)
                       x
                       (* x x)))
             (fn [i] (- (* 2 i) 1))
             k))


(defn test1 []
  (println "tan 10 is"
           (tan-cf 10.0 20)))

