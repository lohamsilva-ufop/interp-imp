(declare-const x Int) (assert (= x 2)) (assert (and (< x  10)(< x  20)))(check-sat) (get-model)
