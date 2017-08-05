(ns sicp.e2_6)

;;
;; exercise 2.6
;;
(def zero (fn [f] (fn [x] (x))))

(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

;;
;; definition of one 
;;
;; (add-1 zero)
;; (fn [f] (fn [x] (f ((zero f) x))))
;; (fn [f] (fn [x] (f ((fn [x] (x)) x))))
;; (fn [f] (fn [x] (f x)))
(def one  (fn [f] (fn [x] (f x))))


;;
;; definition of two 
;;
;; (add-1 one)
;; (fn [f] (fn [x] (f ((one f) x))))
;; (fn [f] (fn [x] (f ((fn [x] (f x)) x)))) 
;; (fn [f] (fn [x] (f (f x))))
(def two  (fn [f] (fn [x] (f (f x)))))


;;
;; direct difinition of add
;;
(defn add [a b]
  (fn [f] (fn [x] ((a f) ((b f) x)))))

;; (add one two)
;; (fn [f] (one (two f)))
;; (fn [f] (one ((fn [x] (f (f x))) x)))
;; (fn [f] ((fn [x] (f x)) (f (f x))))
;; (fn [f] (fn [x] (f (f (f x)))))

(defn print- [x]
  (println ((x inc) 0)))

(print- (add one two))
