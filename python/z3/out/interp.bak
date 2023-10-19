#lang racket

(require "syntax.rkt")

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

(define (eval-stmt env s)
  (match s
    [(define-const-vars ev t v) (print-out-vars ev t v)]
    [(sat-unsat v) (print-out-sat v)]))

(define (eval-stmts env blk)
  (match blk
    ['() env]
    [(cons s blks) (let ([nenv (eval-stmt env s)])
                       (eval-stmts nenv blks))]))

(define (outz3-interp prog)
  (eval-stmts (make-immutable-hash) prog))

(provide outz3-interp)
