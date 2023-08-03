(declare-const x) (assert (and (< x  10)(< x  9)(< x  8)))(check-sat) (get-model)
