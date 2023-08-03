(declare-const x Int) (assert (and (not (< x  10)) )))(check-sat) (get-model)
