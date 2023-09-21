(declare-const numero Int) (declare-const resto Int) (assert (= resto 0)) (assert (not (= resto  0)))(check-sat) (get-model)
