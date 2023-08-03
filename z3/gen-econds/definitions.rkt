#lang racket

(require "../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt"
         "../out/parser.rkt"
         "../gen-evars/gen.rkt")

(require racket/format)

(struct tree-econds (econd true-block else-block) #:transparent)

(define (check-variables econd varlist ctx)
   (match econd
         [(lt e1 e2) (let* ([type (get-type ctx e1)]
                            [par (cons (evar-id e1) type)])
                       (cons par varlist))]))
                    
(define (check-econd node)
  (match node
   [(lt e1 e2)   (string-append "(< " (evar-id e1) "  " (~a (value-value e2)) ")")]))

(define (get-eifs ast)
   (match ast
    ['() '()]
    [(cons (read-v v1) astrest) (get-eifs astrest)]
    [(cons (assign t v e1) astrest) (get-eifs astrest)]
    [(cons (sprint e1) astrest) (get-eifs astrest)]
    [(cons (read-v v1) astrest) (get-eifs astrest)]              
    [(cons (eif econd then-block else-block) astrest) (let* ([tr (get-eifs then-block)]
                                                       [tl (get-eifs else-block)])
                                                       (cons (tree-econds econd tr tl) (get-eifs astrest)))]
    [(cons (ewhile econd block) astrest) (get-eifs astrest)]
    [(cons (efor (assign t v ex) e1 block) astrest) (get-eifs astrest)]))


(provide get-eifs (struct-out tree-econds)) 