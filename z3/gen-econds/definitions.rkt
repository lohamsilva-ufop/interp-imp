#lang racket

(require "../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt"
         "../out/parser.rkt"
         "../gen-evars/gen.rkt"
         "gen-script.rkt")

(require racket/format)

(define (check-variables econd varlist ctx)
   (match econd
         [(lt e1 e2) (let* ([type (get-type ctx e1)]
                            [par (cons (evar-id e1) type)])
                       (cons par varlist))]))

(define (verify-node node)
  (match node
    [(read-v v1) '()]
    [(assign t v e1) '()]
    [(sprint e1) '()]
    [(read-v v1) '()]              
    [(eif econd then-block else-block) (check-econd econd)]
    [(ewhile econd block) '()]
    [(efor (assign t v ex) e1 block) '()]))

(define (check-block block list-then-else)
  (match block
    ['() list-then-else]
    [(cons bl block-res) (begin
                               (let ([new-item-list-then-else (verify-node bl)])
                                 (if (equal? new-item-list-then-else '())
                                     (check-block block-res list-then-else)
                                     (check-block block-res (cons new-item-list-then-else list-then-else)))))]))
                         
(define (check-econd node)
  (match node
   [(lt e1 e2)   (string-append "(< " (evar-id e1) "  " (~a (value-value e2)) ")")]))

(define (check-if econd then-block else-block ctx)
  (let* ([econd-result (check-econd econd)]
          [varlist (check-variables econd '() ctx)]
          [list-then-block (check-block then-block '())]
          [list-else-block (check-block else-block '())])
         (build-script econd-result list-then-block list-else-block varlist))) 

(define (verify-node-program node ctx)
  (match node
    [(read-v v1) '()]
    [(assign t v e1) '()]
    [(sprint e1) '()]
    [(read-v v1) '()]              
    [(eif econd then-block else-block) (check-if econd then-block else-block ctx)]
    [(ewhile econd block) '()]
    [(efor (assign t v ex) e1 block) '()]))

(define (build-script econd list-then-block list-else-block varlist)
  (displayln econd)
  (displayln list-then-block)
  (displayln list-else-block)
  (displayln varlist)
  (gen-script-then-block-true econd list-then-block varlist)
  (gen-script-then-block-false econd list-then-block varlist)
  (gen-script-else-block-true econd list-else-block varlist)
  (gen-script-else-block-false econd list-else-block varlist))


(define (build-ifs-script ast ctx)
(match ast
    ['() (displayln "Finished!")]
    [(cons node ast-res) (begin
                       (verify-node-program node ctx)
                       (build-ifs-script ast-res ctx))]))

(provide build-ifs-script) 