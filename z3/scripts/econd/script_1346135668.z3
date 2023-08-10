(declare-const x Int) (assert (= x 1))(declare-const y Int) (assert (= y 0)) (assert (and (not (< x  10)) (< x  3)))(check-sat) (get-model)
