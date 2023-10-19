#lang racket

(require "../../syntax.rkt")

(require racket/format)

(struct tree-econds (econd true-block else-block) #:transparent)

(define (get-eifs ast)
   (match ast
    ['() '()]
    [(cons (eassign v e1) astrest) (get-eifs astrest)]
    [(cons (sprint e1) astrest) (get-eifs astrest)]
    [(cons (input v1 e1) astrest) (get-eifs astrest)]
    [(cons (eif econd then-block else-block) astrest) (let* ([tr (get-eifs then-block)]
                                                       [tl (get-eifs else-block)])
                                                       (cons (tree-econds econd tr tl) (get-eifs astrest)))] ))

(provide get-eifs (struct-out tree-econds)) 