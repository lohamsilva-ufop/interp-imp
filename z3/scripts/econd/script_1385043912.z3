(declare-const x Int) (assert (and (< x  10) (not (< x  9))))(check-sat) (get-model)
