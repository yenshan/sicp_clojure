(ns sicp.symbolic-differentiation)

(defn variable? [x] (symbol? x))

(defn =number? [exp number]
  (and (number? exp) (= exp number)))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

(defn sum? [x]
  (and (coll? x) (= (first x) '+)))

(defn addend [x] (first (rest x)))

(defn augend [x] (first (rest (rest x))))

(defn product? [x]
  (and (coll? x) (= (first x) '*)))

(defn multiplier [x] (first (rest x)))

(defn multiplicand [x] (first (rest (rest x))))

(defn make-exponentiation [a1 a2]
  (cond (=number? a2 0) 1
        (=number? a2 1) a1
        :else (list '** a1 a2)))

(defn base [e] (first (rest e)))
(defn exponent [e] (first (rest (rest e))))
(defn exponentiation? [e]
  (and (coll? e) (= (first e) '**)))

(defn deriv [exp variable]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp variable) 1 0)
        (sum? exp) (make-sum (deriv (addend exp) variable)
                              (deriv (augend exp) variable))
        (product? exp) (make-sum (make-product (multiplier exp)
                                                 (deriv (multiplicand exp) variable))
                                  (make-product (deriv (multiplier exp) variable)
                                                 (multiplicand exp)))
        (exponentiation? exp)
          (make-product 
            (make-product (exponent exp)
                           (make-exponentiation (base exp) (dec (exponent exp))))
            (deriv (base exp) variable))
        :else (println "unkown expression type -- DERIV" exp)))

