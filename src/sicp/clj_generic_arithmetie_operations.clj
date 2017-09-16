(ns sicp.clj-generic-arithmetie-operations
  (:require [sicp.gcd :refer [gcd]])
  (:require [sicp.clj-complex-number :refer :all])
  (:require [clojure.test :refer :all]))

;;
;; Implementation of Generic Arithmetie Operatoins in Clojure style
;;


(defmulti add (fn [x y] [(class x) (class y)]))
(defmulti sub (fn [x y] [(class x) (class y)]))
(defmulti mul (fn [x y] [(class x) (class y)]))
(defmulti div (fn [x y] [(class x) (class y)]))
(defmulti equ? (fn [x y] [(class x) (class y)]))
(defmulti raise (fn [n] (class n))) 
(defmulti =zero? (fn [n] (class n))) 

(defmulti ->list (fn [d] (class d)))

;;
;; scheme number package
;;
(defrecord SchemeNumber [dat])

(defn make-scheme-number [n] (->SchemeNumber n))

(defmethod ->list SchemeNumber [e]
  (list 'SchemeNumber (:dat e)))

(defmethod add [SchemeNumber SchemeNumber]
  [{x :dat} {y :dat}]
  (make-scheme-number (+ x y)))

(defmethod sub [SchemeNumber SchemeNumber]
  [{x :dat} {y :dat}]
  (make-scheme-number (- x y)))

(defmethod mul [SchemeNumber SchemeNumber]
  [{x :dat} {y :dat}]
  (make-scheme-number (* x y)))

(defmethod div [SchemeNumber SchemeNumber]
  [{x :dat} {y :dat}]
  (make-scheme-number (/ x y)))

(defmethod equ? [SchemeNumber SchemeNumber]
  [{x :dat} {y :dat}]
  (= x y))

(defmethod =zero? SchemeNumber
  [{x :dat}]
  (zero? x))

;;
;; rational number package
;;
(defrecord Rational [numer denom])

(defmethod ->list Rational [d]
  (list (:numer d) (:denom d)))

(defn- make-rational [n d]
  (let [g (gcd n d)]
    (->Rational (/ n g) (/ d g))))

(defmethod add [Rational Rational] [x y]
  (make-rational (+ (* (:numer x) (:denom y))
                    (* (:numer y) (:denom x)))
                 (* (:denom x) (:denom y))))

(defmethod sub [Rational Rational] [x y]
  (make-rational (- (* (:numer x) (:denom y))
                    (* (:numer y) (:denom x)))
                 (* (:denom x) (:denom y))))

(defmethod mul [Rational Rational] [x y]
  (make-rational (* (:numer x) (:numer y))
                 (* (:denom x) (:denom y))))

(defmethod div [Rational Rational] [x y]
  (make-rational (* (:numer x) (:denom y))
                 (* (:denom x) (:numer y))))

(defmethod equ? [Rational Rational] [x y]
  (and (= (:numer x) (:numer y))
       (= (:denom x) (:denom y))))


;;
;; real number
;;
(defrecord RealNumber [dat])

(defn make-real-number [n]
  (->RealNumber (double n)))

(defmethod add [RealNumber RealNumber]
  [{x :dat} {y :dat}]
  (make-real-number (+ x y)))

;;
;;
;;
(defrecord Complex [dat])

(defn make-complex-from-real-imag [x y]
  (->Complex (make-from-real-imag x y)))


;;
;;
;;
(defmethod raise SchemeNumber
  [{n :dat}]
  (make-rational n 1))

(defmethod raise Rational
  [{n :numer}]
  (make-real-number n))

(defmethod raise RealNumber
  [{n :dat}]
  (make-complex-from-real-imag n 0))

(defn <? [t1 t2]
  (let [tbl {Complex #{}
             RealNumber #{Complex}
             Rational #{RealNumber Complex}
             SchemeNumber #{Rational RealNumber Complex}}]
    (-> (get tbl t1)
        (contains? t2))))

(defmethod add :default [x y]
  (let [tx (class x)
        ty (class y)]
    (if (<? tx ty)
      (add (raise x) y)
      (add (x (raise y))))))

;;
;;
;;
(deftest test-main
  (testing "test add"
    (is (= 3 (:dat (add (make-scheme-number 1) (make-scheme-number 2)))))
    (is (= 5.0 (:dat (add (make-real-number 2) (make-real-number 3)))))
    (is (= '(7 6) (->list (add (make-rational 1 2) (make-rational 2 3)))))
    )
  (testing "test <?"
    (is (= true (<? (class (make-scheme-number 1)) (class (make-rational 1 2)))))
  )
  (testing "test raise"
    (is (= (make-rational 10 1) (raise (make-scheme-number 10))))
    (is (= (make-real-number 10.0) (raise (raise (make-scheme-number 10)))))
    (is (= (make-complex-from-real-imag 10.0 0) (raise (raise (raise (make-scheme-number 10))))))
  )
  (testing "test add with different type of argument"
    (is (= (make-real-number 5) (add (make-scheme-number 2) (make-real-number 3))))
  )
  )


