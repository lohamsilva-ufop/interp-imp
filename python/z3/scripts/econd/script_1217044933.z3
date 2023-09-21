(declare-const A Int) (declare-const B Int)  (assert (not (> A  B)))(check-sat) (get-model)
