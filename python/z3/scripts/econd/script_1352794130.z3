(declare-const salario Int) (declare-const prestacao Int) (declare-const proporcao Int) (assert (= proporcao 435)) (assert (< prestacao  proporcao))(check-sat) (get-model)
