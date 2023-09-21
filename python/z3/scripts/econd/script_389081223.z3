(declare-const numero Int)  (assert (not (>= numero  0)))(check-sat) (get-model)
