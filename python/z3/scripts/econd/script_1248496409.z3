(declare-const numero Int) (declare-const divisivel Int) (assert (= divisivel 0)) (assert (not (= divisivel  0)))(check-sat) (get-model)
