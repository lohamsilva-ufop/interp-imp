(declare-const numero Int)  (assert (and (not (> numero  0)) (not (< numero  0)) ))(check-sat) (get-model)
