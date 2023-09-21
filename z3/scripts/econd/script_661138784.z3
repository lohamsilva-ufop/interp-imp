(declare-const x Int) (assert (= x 1))(declare-const sum Int) (assert (= sum 10)) (assert (and (< x  10)(< x  9)(< x  8)))(check-sat) (get-model)
