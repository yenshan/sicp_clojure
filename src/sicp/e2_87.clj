(ns sicp.e2_87
  (:require [clojure.test :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all])
  (:require [sicp.arithmetic-polynomials :refer :all]))

;;
;; =zero? is added in arithmetic_polynomial.clj
;;

(deftest test-e2_87
  (let [_ (install-scheme-number-package)
        _ (install-polynomial-package)]
    (testing "test =zero?"
      (is (=zero? (make-polynomial 'x '((2 (scheme-number 0)) (1 (scheme-number 0))))))
      (is (not (=zero? (make-polynomial 'x '((2 (scheme-number 0)) (1 (scheme-number 1)))))))
      )
    ))
