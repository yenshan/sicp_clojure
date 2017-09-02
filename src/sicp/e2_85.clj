(ns sicp.e2_85
  (:require [sicp.e2-83 :refer [raise]])
  (:require [clojure.test :refer :all])
  (:require [sicp.combining-data-of-different-type :refer :all])
  (:require [sicp.data-directed-lib :refer [attach-tag
                                            type-tag 
                                            contents 
                                            tput 
                                            tget]])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer [install-complex-package
                                                        install-scheme-number-package
                                                        install-rational-package
                                                        install-real-number-package
                                                        make-complex-from-real-imag
                                                        make-scheme-number
                                                        make-real-number
                                                        make-rational
                                                        ->double
                                                        equ?]])
  )


(defn project [x]
  (condp = (type-tag x)
    'complex (make-real-number (real-part (contents x)))
    'real-number (make-rational (Math/round (double (contents x))) 1)
    'rational (make-scheme-number (Math/round (->double x)))
    nil))
                                  
(defn drop-num [x]
  (let [low (project x)]
    (if (and low (equ? x (raise low)))
      (drop-num low)
      x)))
 

(deftest test-e2-85
  (let [_ (install-scheme-number-package)
        _ (install-rational-package)
        _ (install-real-number-package)
        _ (install-polar-package)
        _ (install-rectangular-package)
        _ (install-complex-package)
        ]
  (testing "test project"
    (is (= '(real-number 2.0) (project (make-complex-from-real-imag 2 30))))
    (is (= '(rational [2 1]) (project (project (make-complex-from-real-imag 2 30)))))
    (is (= '(real-number 1.0) (raise (project (project (make-complex-from-real-imag 1 0))))))
    )
  (testing "test drop-num"
    (is (= '(complex (rectangular (1 20))) (drop-num (make-complex-from-real-imag 1 20))))
    (is (= '(scheme-number 1) (drop-num (make-complex-from-real-imag 1 0))))
    )
  ))

