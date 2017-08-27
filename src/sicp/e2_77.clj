(ns sicp.e2_77
  (:require [sicp.complex-number :refer [real-part imag-part magnitude angle install-rectangular-package install-polar-package]])
  (:require [sicp.data-directed-lib :refer :all])
  (:require [sicp.generic-arithmetie-operations :refer :all]))


(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(tput 'magnitude '(complex) magnitude)

(def dat (make-complex-from-real-imag 1 2))

(complex (rectangular (1 2)))

(magnitude dat)
; -> (apply-generic 'magnitude z)
;    -> (apply magnitude (rectangular (1 2))) -> magnitude (complex)
;       -> (magnutude '(rectangular (1 2)))
;          -> (apply-generic 'magnitude z)
;              -> (apply magnitude (1 2))     -> magnitude (rectangular)

