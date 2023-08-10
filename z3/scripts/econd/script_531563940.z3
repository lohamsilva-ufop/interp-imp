(declare-const x Int)(assert (=x 1)) (assert (and (< x  10)(< x  20)))(check-sat) (get-model)
