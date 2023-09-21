(declare-const numero Int)  (assert (and (not (> numero  0)) (not (= numero  0)) (not (= numero  7)) ))(check-sat) (get-model)
