(ns sicp.combining-data-of-different-type
  (:require [sicp.data-directed-lib :refer [type-tag contents tput tget]])
  (:require [sicp.complex-number :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer [install-complex-package
                                                        install-scheme-number-package
                                                        make-complex-from-real-imag
                                                        make-scheme-number]])
  (:require [clojure.test :refer :all]))

(defn scheme-number->complex [n]
  (make-complex-from-real-imag (contents n) 0))


(defn put-coercion [type1 type2 proc]
  (tput 'type-cast (list type1 type2) proc))

(defn get-coercion [type1 type2]
  (tget 'type-cast (list type1 type2)))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (tget op type-tags)]
    (if proc 
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [type1 (first type-tags)
              type2 (second type-tags)
              a1 (first args)
              a2 (second args)
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
          (cond t1->t2 (apply-generic op (t1->t2 a1) a2)
                t2->t1 (apply-generic op a1 (t2->t1 a2))
                :else (println "No method for these types" (list op type-tags))))
        (println "No method for these types" (list op type-tags))))))

(defn add [x y] (apply-generic 'add x y))

(defn install-coercion-package []
  (put-coercion 'scheme-number 'complex scheme-number->complex))
      
(deftest test-main
  (let [_ (install-scheme-number-package)
        _ (install-polar-package)
        _ (install-rectangular-package)
        _ (install-complex-package)
        _ (install-coercion-package)]
    (testing "test get-coercion"
      (is (not= nil (get-coercion 'scheme-number 'complex)))
      (is (= nil (get-coercion 'scheme-number 'scheme-number)))
      )
    (testing "test scheme-number->complex"
      (is (= '(complex (rectangular (1 0))) (scheme-number->complex (make-scheme-number 1))))
      )
    (testing "test add complex scheme" 
      (is (= '(complex (rectangular (13 2))) (add (make-scheme-number 12)
                                                 (make-complex-from-real-imag 1 2))))
      ;(is (= nil (add (make-scheme-number 12) (make-scheme-number 10))))
      )))

