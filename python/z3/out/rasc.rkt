#lang racket
(define (print-out-vars ev t v)
  (begin
  (display "Variável: ")
  (displayln (evar-id ev))
  (display "Tipo: ")
  (displayln (type-value t))
  (display "Valor Gerado: ")
  (displayln (value-value v))
  (displayln "=====================")))

(define (print-out-sat v)
  (if (equal? 'sat v)
  (begin
  (displayln "Satisfatível")
  (displayln "====================="))

  (begin
  (displayln "Não Satisfatível ")
  (displayln "====================="))))