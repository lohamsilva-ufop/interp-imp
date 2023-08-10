(declare-const x Int)(assert (= x 1)) (assert (and (not (< x  10)) (not (< x  3))))(check-sat) (get-model)
