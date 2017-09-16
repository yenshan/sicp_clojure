(ns sicp.e2_90.termlist-interface)

(defmulti ->vec (fn [e] (class e)))
(defmulti add-terms (fn [e1 e2] [(class e1) (class e2)]))
(defmulti mul-terms (fn [e1 e2] [(class e1) (class e2)]))
