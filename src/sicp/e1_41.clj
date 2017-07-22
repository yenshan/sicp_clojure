(ns sicp.e1_41)

(defn double- [f]
  (fn [g] (f (f g)))) 

(defn e1_41-test []
  (println "result of (((double (double double)) inc) 5) is"
           (((double- (double- double-)) inc) 5)))

