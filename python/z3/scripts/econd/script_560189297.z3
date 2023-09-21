(declare-const numero Int)  (assert (not (> numero  20)))(check-sat) (get-model)
