(assert (and (< x 10) (< x 5)))
(assert (and (< x 10) (not (< x 5)))) 
(assert (and (not (< x 10)) (< x 3)))
(assert (and (not (< x 10)) (not (< x 3))))

(assert (and (< x  10) (< x  5))(check-sat) (get-model)