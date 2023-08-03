(declare-const x Int) (assert (and (not (< x  10)) (not (< x  6)(< x  7))))(check-sat) (get-model)
