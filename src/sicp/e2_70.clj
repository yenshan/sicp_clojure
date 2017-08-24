(ns sicp.e2_70
  (:require [clojure.test :refer :all])
  (:require [clojure.string :as s])
  (:require [sicp.e2_68 :refer [encode]])
  (:require [sicp.e2_69 :refer [generate-huffman-tree]]))


(def tree (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(def message "Get a job
             Sha na na na na na na na na
             Get a job
             Sha na na na na na na na na
             Wah yip yip yip yip yip yip yip yip yip
             Sha boom")

(defn- split [reg string] (s/split string reg))

(def message-list (->> message
                       s/upper-case
                       (split #"[ |\n]")
                       (filter #(not= "" %))
                       (map #(symbol %))))

(deftest test-e2_70
  (testing "How many bits are required for the encoding?"
    (is (= 123 (count (encode message-list tree))))
    ))
