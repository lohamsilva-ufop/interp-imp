(declare-const numero Int) (declare-const multiplo Int) (assert (= multiplo 0)) (assert (not (= multiplo  0)))(check-sat) (get-model)
