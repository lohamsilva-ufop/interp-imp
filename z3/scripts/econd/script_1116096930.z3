(declare-const x Int) (assert (and (not(< x  10)) (< x  6)))(check-sat) (get-model)
