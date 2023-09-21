(declare-const A Int) (declare-const B Int) (declare-const C Int)  (assert (and ( and ( < A B) ( < A C))  ( > B C)))(check-sat) (get-model)
