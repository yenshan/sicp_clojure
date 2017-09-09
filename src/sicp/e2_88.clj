(ns sicp.e2_88
  (:require [clojure.test :refer :all])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all])
  (:require [sicp.arithmetic-polynomials :refer :all]))


(defn- neg-terms [term-list]
  (map (fn [t]
         (make-term (order t)
                    (negate (coeff t))))
       term-list))

(defn- sub-terms [L1 L2]
  (add-terms L1 (neg-terms L2)))

(defn- sub-poly [p1 p2]
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (sub-terms (term-list p1)
                          (term-list p2)))
    (println "Polys not in same var -- SUB-POLY" (list p1 p2))))


(deftest test-e2_88
  (let [_ (install-scheme-number-package)
        _ (install-rational-package)
        _ (install-real-number-package)
        _ (install-rectangular-package)
        _ (install-polar-package)
        _ (install-complex-package)
        ]
  (testing "test negate"
    (is (= '(scheme-number -2) (negate (make-scheme-number 2))))
    (is (= '(rational [-1 2]) (negate (make-rational 1 2))))
    (is (= '(real-number -1.0) (negate (make-real-number 1))))
    (is (= '(complex (rectangular (-1 2))) (negate (make-complex-from-real-imag 1 2))))
    )
  (testing "test neg-terms"
    (is (= '((3 (scheme-number -2)))  (neg-terms '((3 (scheme-number 2))))))
    (is (= '((3 (scheme-number -2)) (4 (scheme-number -2)))  (neg-terms '((3 (scheme-number 2)) (4 (scheme-number 2))))))
    )
  (testing "test sub-terms"
    (is (= '((2 (scheme-number 2))) 
           (sub-terms '((2 (scheme-number 3))) '((2 (scheme-number 1))))))
  )
  (testing "test sub-poly"
    (is (= '(x ((3 (scheme-number -2)) (2 (scheme-number 3))))
           (sub-poly 
             (second (make-polynomial 'x '((2 (scheme-number 3)))))
             (second (make-polynomial 'x '((3 (scheme-number 2))))))))
  )
  )
  )
