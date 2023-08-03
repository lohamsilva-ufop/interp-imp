(declare-const x Int) (assert (and (not (< x  10)(< x  9)(< x  8))))(check-sat) (get-model)
