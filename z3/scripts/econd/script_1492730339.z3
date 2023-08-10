(declare-const x Int) (assert (= x 1))(declare-const z Int) (assert (= z 10)) (assert (and (not (< x  10)) (< x  3)))(check-sat) (get-model)
