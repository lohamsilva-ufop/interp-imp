(declare-const x Int) (assert (and (< x  10)))(check-sat) (get-model)
