(declare-const A Int) (declare-const B Int)  (assert (> #(struct:evar A)  #(struct:evar B)))(check-sat) (get-model)
