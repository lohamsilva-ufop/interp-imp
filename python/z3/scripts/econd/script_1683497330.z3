(declare-const numero Int) (declare-const multiplo Int) (assert (= multiplo ( mod numero 3))) (assert (= multiplo  0))(check-sat) (get-model)
