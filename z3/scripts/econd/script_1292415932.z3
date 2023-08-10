(declare-const x Int) (assert (= x 1))(declare-const y Int) (assert (= y 0)) (assert (and (< x  10)  (not (and (< x  9)(< x  8)))))(check-sat) (get-model)
