(declare-const numero Int)  (assert (and (> numero  0) (not (> numero  3)) ))(check-sat) (get-model)
