(declare-const salario Int) (declare-const prestacao Int) (declare-const proporcao Int) (assert (= proporcao ( / ( * salario 30) 100))) (assert (not (< prestacao  proporcao)))(check-sat) (get-model)
