(declare-const numero Int)  (assert (and (not (= numero  5)) (= numero  20)(= numero  400)(and ( >= numero 500)  ( <= numero 1000))))(check-sat) (get-model)
