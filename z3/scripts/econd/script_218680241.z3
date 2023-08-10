(declare-const x Int) (assert (= x 1))(declare-const y Int) (assert (= y 9)) (assert (not (< x  10)))(check-sat) (get-model)
