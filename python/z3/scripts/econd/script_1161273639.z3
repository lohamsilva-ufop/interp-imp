(declare-const v1 Int) (declare-const v2 Int) (declare-const soma Int) (assert (= soma 40)) (assert (> soma  10))(check-sat) (get-model)
