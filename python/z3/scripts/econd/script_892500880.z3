(declare-const A Int) (declare-const B Int) (declare-const divisivel Int) (assert (= divisivel ( mod A B))) (assert (not (= divisivel  0)))(check-sat) (get-model)
