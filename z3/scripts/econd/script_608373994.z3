(declare-const x Int) (assert (and (< x  10)(< x  20)))(check-sat) (get-model)
