(declare-const v1 Int) (declare-const v2 Int) (declare-const soma Int) (assert (= soma ( + v1 v2))) (assert (> soma  10))(check-sat) (get-model)
