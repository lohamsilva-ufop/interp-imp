(declare-const x Int) (assert (and (< x  10) (not (< x  20))))(check-sat) (get-model)
