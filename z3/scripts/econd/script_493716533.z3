(declare-const x Int)(assert (= x 1)) (assert (and (not (< x  10)) (and (< x  3))))(check-sat) (get-model)
