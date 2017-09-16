(ns sicp.e2_90.termlist-interface)

(defmulti constructor (fn [e] (class e)))

(def empty-term? (comp empty? :dat))

(def first-term (comp first :dat))

(defn rest-terms [term-list]
  ((constructor term-list) (rest (:dat term-list))))

(defmulti add-terms (fn [e1 e2] [(class e1) (class e2)]))

(defmulti mul-terms (fn [e1 e2] [(class e1) (class e2)]))
