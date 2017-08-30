(ns sicp.e2_79
  (:require [clojure.test :refer :all])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all]))


;;
;; equ? is added in generic_arithmetie-operations.clj
;;
(deftest test-e2_79
  (testing "test equ?"
    (let [_ (install-scheme-number-package)
          _ (install-rational-package)
          _ (install-rectangular-package)
          _ (install-polar-package)
          _ (install-complex-package)]
      (is (equ? (make-scheme-number 1) (make-scheme-number 1)))
      (is (not (equ? (make-scheme-number 1) (make-scheme-number 3))))
      (is (equ? (make-rational 1 2) (make-rational 1 2)))
      (is (not (equ? (make-rational 1 2) (make-rational 2 1))))
      (is (equ? (make-complex-from-mag-ang 1 20)
                (make-complex-from-mag-ang 1 20)))
      (is (not (equ? (make-complex-from-mag-ang 1 20)
                     (make-complex-from-mag-ang 2 200))))
      (is (equ? (make-from-real-imag 1 2)
                (make-from-real-imag 1 2)))
      (is (not (equ? (make-from-real-imag 1 2)
                     (make-from-real-imag 2 2))))
    )))
