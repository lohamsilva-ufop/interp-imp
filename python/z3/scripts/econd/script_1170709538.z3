(declare-const numero Int)  (assert (not (and ( >= numero 20)  ( <= numero 90))))(check-sat) (get-model)
