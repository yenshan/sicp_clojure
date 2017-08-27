(ns sicp.data-directed-lib)

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
  (swap! table-operations assoc-in [key1 key2] func))

(defn tget [key1 key2]
  (get-in @table-operations [key1 key2]))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (tget op type-tags)]
    (if proc
      (apply proc (map contents args))
      (println "No method for these types -- APPLY-GENERIC"
               (list op type-tags)))))
