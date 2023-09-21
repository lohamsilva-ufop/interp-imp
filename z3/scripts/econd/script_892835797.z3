(declare-const x Int) (assert (= x 1)) (assert (< x  10))(check-sat) (get-model)
