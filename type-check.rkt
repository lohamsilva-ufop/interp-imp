#lang racket

(require "syntax.rkt")

(define (type-check-expr ctx e)
    (match e
      [(value e1) (cond
                   [(number? e1) 'int ]
                   [(boolean? e1) 'boolean]
                   [else (error "Type Error")])]

      [(evar e1) e1]

      [(add e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'int]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
      [(minus e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'int]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
      [(mult e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'int]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
      [(divv e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'int]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
       [(lt e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'boolean]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
       [(eeq e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'boolean]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro.")])]
      [(eand e1 e2) (cond
                     [(eq? (type-check-expr ctx e1) (type-check-expr ctx e2)) 'boolean]
                     [else (error "A expressão espera dois valores boleanos, mas um dos ou os valores passados não correspondem a um boleano.")])]
      [(enot e1)    (cond
                     [(eq? (type-check-expr ctx e1) 'boolean) 'boolean]
                     [else (error "A expressão espera um valor booleano, porém o valor passado não corresponde a um boleano.")])]
    )
 )

(define (type-check-stmt ctx s)
  (match s
    [(assign t v e1)
       (let* ([expr (type-check-expr ctx e1)]
              [type-v (type-value t)]
              [variable (type-check-expr ctx v)])
             
                (if (eq? expr type-v)
                      (hash-set ctx variable type-v)
                      (error "A expressão não corresponde com o tipo da variável")))]       
    [(sprint e1)
       (let ([tl (type-check-expr ctx e1)])
             ctx)]
    ;[(eif econd then-block else-block)
    ;    (let ([expr (type-check-expr ctx econd)])
        ;     (if (eq? expr 'boolean)



         ; )



    ; ]



    ))

;ctx: contexto para tipos
(define (type-check-stmts ctx blk)
  (match blk
    ['() ctx]
    [(cons s blks) (let ([nctx (type-check-stmt ctx s)])
                      (begin
                        ;(displayln nctx)
                       (type-check-stmts nctx blks)))]))

(define (type-check prog)
  (type-check-stmts (make-immutable-hash) prog))

(provide type-check type-check-expr)