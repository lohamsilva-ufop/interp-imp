(declare-const x Int) (assert (and (< x  10) (< x  9)))(check-sat) (get-model)
