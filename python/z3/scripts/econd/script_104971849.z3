(declare-const A Int) (declare-const B Int) (declare-const C Int) (declare-const D Int)  (assert (and ( and ( < A B) ( < A C))  ( > A D)))(check-sat) (get-model)
