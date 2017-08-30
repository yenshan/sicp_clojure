(ns sicp.e2_80
  (:require [clojure.test :refer :all])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all]))


;;
;; =zero? is added in generic_arithmetie-operations.clj
;;
(deftest test-e2_80
  (testing "test =zero?"
    (let [_ (install-scheme-number-package)
          _ (install-rational-package)
          _ (install-rectangular-package)
          _ (install-polar-package)
          _ (install-complex-package)]
      (is (=zero? (make-scheme-number 0)))
      (is (not (=zero? (make-scheme-number 1))))
      (is (=zero? (make-rational 0 2)))
      (is (not (=zero? (make-rational 1 2))))
      (is (=zero? (make-complex-from-mag-ang 0 20)))
      (is (not (=zero? (make-complex-from-mag-ang 1 20))))
      (is (=zero? (make-from-real-imag 0 0)))
      (is (not (=zero? (make-from-real-imag 1 2))))
    )))
