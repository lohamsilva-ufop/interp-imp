(declare-const numero Int) (declare-const divisivel Int) (assert (= divisivel ( mod numero 5))) (assert (not (= divisivel  0)))(check-sat) (get-model)
