(declare-const v1 Int) (declare-const v2 Int) (declare-const soma Int) (assert (= soma ( + #(struct:evar v1) #(struct:evar v2)))) (assert (> soma  10))(check-sat) (get-model)
