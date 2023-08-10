(declare-const x Int) (assert (= x 2)) (assert (and (not (< x  10)) (not (< x  6))))(check-sat) (get-model)
