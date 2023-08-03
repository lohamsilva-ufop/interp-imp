(declare-const x Int) (assert (and (not (< x  10)) (< x  6)(< x  7)))(check-sat) (get-model)
