(declare-const numero Int)  (assert (and ( >= numero 20)  ( <= numero 90)))(check-sat) (get-model)
