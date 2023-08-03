(declare-const x Int) (assert (and (< x  10)(< x  20)(< x  7)))(check-sat) (get-model)
