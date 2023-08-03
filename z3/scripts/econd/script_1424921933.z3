(declare-const x Int) (assert (and (not (< x  1) 0)(< x  3))))(check-sat) (get-model)
