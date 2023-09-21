(declare-const A Int) (declare-const B Int) (declare-const C Int)  (assert (and ( > A B)  ( > A C)))(check-sat) (get-model)
