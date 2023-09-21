(declare-const numero Int)  (assert (and (not (> numero  0)) (= numero  0)(= numero  7)))(check-sat) (get-model)
