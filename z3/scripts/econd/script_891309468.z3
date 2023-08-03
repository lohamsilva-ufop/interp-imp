(declare-const x Int) (assert (not (< x  10)))(check-sat) (get-model)
