(ns sicp.e2-83
  (:require [sicp.combining-data-of-different-type :refer :all])
  (:require [sicp.data-directed-lib :refer [attach-tag type-tag contents tput tget]])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer [install-complex-package
                                                        install-scheme-number-package
                                                        install-rational-package
                                                        make-complex-from-real-imag
                                                        make-scheme-number
                                                        make-rational]])
  (:require [clojure.test :refer :all]))

(defn ->double [x]
  ((tget '->double '(rational)) x))
  
;;
;; real number package
;;
(defn install-real-number-package []
  (letfn [(tag [x]
            (attach-tag 'real-number x))]
    (tput 'add '(real-number real-number)
      (fn [x y] (tag (+ x y))))
    (tput 'sub '(real-number real-number)
      (fn [x y] (tag (- x y))))
    (tput 'make 'real-number
      (fn [x] (tag x))))
)

(defn make-real-number [n]
  ((tget 'make 'real-number) n))

;;
;; generic raise function
;;
(defn raise [x]
  (condp = (type-tag x)
    'scheme-number (make-rational (contents x) 1)
    'rational (make-real-number (->double x))
    'real-number (make-complex-from-real-imag (contents x) 0)
    :else x))


(deftest test-e2-83
  (let [_ (install-scheme-number-package)
        _ (install-rational-package)
        _ (install-real-number-package)
        _ (install-polar-package)
        _ (install-rectangular-package)
        _ (install-complex-package)
        ]
    (is (= '(rational [9 1]) (raise (make-scheme-number 9))))
    (is (= '(real-number 9.0) (raise (raise (make-scheme-number 9)))))
    (is (= '(complex (rectangular (9.0 0))) (raise (raise (raise (make-scheme-number 9))))))
    ))
