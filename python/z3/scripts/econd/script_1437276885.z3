(declare-const numero Int)  (assert (and (> numero  0) (> numero  3)))(check-sat) (get-model)
