(declare-const numero Int)  (assert (and (not (> numero  0)) (< numero  0)))(check-sat) (get-model)
