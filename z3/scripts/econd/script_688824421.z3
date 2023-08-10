(declare-const x Int) (assert (= x 8)) (assert (and (< x  10)  (not (and (< x  20)))))(check-sat) (get-model)
