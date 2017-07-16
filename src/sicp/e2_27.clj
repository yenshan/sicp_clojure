(ns sicp.e2_27
  (gen-class))

;;
;; Deep reverse
;;

(defn deep-reverse
  ([coll] (deep-reverse coll (list)))
  ([coll result]
   (if (empty? coll)
     result
     (let [elem (first coll)
           elem' (if (coll? elem) (deep-reverse elem) elem)]
       (recur (rest coll) (cons elem' result))))))
