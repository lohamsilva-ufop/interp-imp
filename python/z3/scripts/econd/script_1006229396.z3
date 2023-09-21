(declare-const A Int) (declare-const B Int) (declare-const C Int)  (assert (and (not (and ( < A B)  ( < A C))) (not (and ( < B A)  ( < B C))) ))(check-sat) (get-model)
