(declare-const numero Int) (declare-const resto Int) (assert (= resto ( mod numero 2))) (assert (not (= resto  0)))(check-sat) (get-model)
