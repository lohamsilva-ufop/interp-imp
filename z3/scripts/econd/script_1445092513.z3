(declare-const x Int) (assert (= x 1))(declare-const y Int) (assert (= y 9))(declare-const z Int) (assert (= z 9)) (assert (< x  10))(check-sat) (get-model)
