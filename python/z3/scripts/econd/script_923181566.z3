(declare-const A Int) (declare-const B Int)  (assert (not (> #(struct:evar A)  #(struct:evar B))))(check-sat) (get-model)
