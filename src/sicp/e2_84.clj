(ns sicp.e2-84
  (:require [sicp.combining-data-of-different-type :refer :all])
  (:require [sicp.data-directed-lib :refer [attach-tag type-tag contents tput tget]])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer [install-complex-package
                                                        install-scheme-number-package
                                                        install-rational-package
                                                        make-complex-from-real-imag
                                                        make-scheme-number
                                                        make-rational]])
  (:require [sicp.e2-83 :refer [raise make-real-number install-real-number-package]])
  (:require [clojure.test :refer :all]))


(defn <type? [t1 t2]
  (let [tbl {'complex #{}
             'real-number #{'complex}
             'rational #{'real-number 'complex}
             'scheme-number #{'rational 'real-number 'complex}}]
    (-> (get tbl t1)
        (contains? t2))))


(defn apply-generic2 [op & args]
  (let [type-tags (map type-tag args)
        proc (tget op type-tags)]
    (if proc 
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)]
          (if (<type? type1 type2)
            (apply-generic2 op (raise a1) a2)
            (apply-generic2 op a1 (raise a2))))
        (println "No method for these types" (list op type-tags))))))

(deftest test-e2-84
  (let [_ (install-scheme-number-package)
        _ (install-rational-package)
        _ (install-real-number-package)]
    (testing "test <type?"
      (is (= true (<type? 'scheme-number 'real-number)))
      (is (= true (<type? 'scheme-number 'rational)))
      (is (= true (<type? 'real-number 'complex)))
      (is (= true (<type? 'rational 'real-number)))
      (is (= true (<type? 'rational 'complex)))
      )
    (testing "test apply-generic2"
      (is (= '(rational [23 2]) (apply-generic2 'add 
                                                (make-scheme-number 10)
                                                (make-rational 3 2))))
      (is (= '(real-number 3.5) (apply-generic2 'add 
                                                (make-rational 3 2)
                                                (make-real-number 2.0))))


      )
  ))
  
