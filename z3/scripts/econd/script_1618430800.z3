(declare-const x Int) (assert (and (< x  1  (not 0)(< x  9)(< x  8))))(check-sat) (get-model)
