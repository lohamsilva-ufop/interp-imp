#lang racket

(require "syntax.rkt")

;premissa1: expressao reduz ao valor
;premissa2: um novo ambiente é a composição do ambiente união (par - identificador e valor)
;a atribuição reduz para um novo ambiente (new-env)
(define (eval-assign env v e)
  (let* ([expr-result (eval-expr env e)]
        [new-env (hash-set env (evar-id v) expr-result)])
    new-env))

;premissa: expressão reduz a um valor
;conclusao: reduz ao ambiente
(define (search-value env v)
  (let ([value (hash-ref env (evar-id v))]) value))

;para todas as expressões:
;premissas: expressão reduz a um valor
;conclusoes: operações sobre os valores.
(define (eval-expr env e)
  (match e
   [(add e1 e2)  (+ (eval-expr env e1) (eval-expr env e2))]
   [(minus e1 e2)(- (eval-expr env e1) (eval-expr env e2))]
   [(mult e1 e2) (* (eval-expr env e1) (eval-expr env e2))]
   [(divv e1 e2) (/ (eval-expr env e1) (eval-expr env e2))]
   [(lt e1 e2)   (< (eval-expr env e1) (eval-expr env e2))]
   [(eeq e1 e2)  (eq? (eval-expr env e1) (eval-expr env e2))]
   [(eand e1 e2) (and (eval-expr env e1) (eval-expr env e2))]
   [(enot e1) (not (eval-expr env e1))]
   [(evar e1) (search-value env (evar e1))]
   [(value val) val]))

(define (eval-stmt env s)
  (match s
    [(assign t v e1) (eval-assign env v e1)]
    [(sprint e1)
     (let ([v (eval-expr env e1)])
       (begin
         (displayln  v)
         env))]))

(define (eval-stmts env blk)
  (match blk
    ['() env]
    [(cons s blks) (let ([nenv (eval-stmt env s)])
                       (eval-stmts nenv blks))]))

(define (imp-interp prog)
  (eval-stmts (make-immutable-hash) prog))

(provide imp-interp eval-expr)
