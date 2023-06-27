#lang racket

(require "syntax.rkt")
(define (get-type ctx v)
  (let ([type (hash-ref ctx (evar-id v))]) type))

(define (check-value-or-variable ctx e)
  (match e
      [(evar e1) (hash-ref ctx e1)]
      [(value e1) (type-check-expr ctx e)]
      [(add e1 e2) (type-check-expr ctx e)]
      [(minus e1 e2) (type-check-expr ctx e)]
      [(mult e1 e2) (type-check-expr ctx e)]
      [(divv e1 e2) (type-check-expr ctx e)]
      [(lt e1 e2) (type-check-expr ctx e)]
      [(eeq e1 e2) (type-check-expr ctx e)]
      [(eand e1 e2) (type-check-expr ctx e)]
      [(enot e1) (type-check-expr ctx e)]))

(define (type-check-expr ctx e)
    (match e
      [(value e1) (cond
                   [(number? e1) 'int ]
                   [(boolean? e1) 'boolean]
                   [else (error "Erro de tipo.")])]
      [(evar e1) e1]
      [(add e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'int]
                     [else (error "A expressão espera valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro (Exp: SOMA).")])]
      [(minus e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'int]
                     [else (error "A expressão espera valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro (Exp: SUBTRAÇÃO).")])]
      [(mult e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'int]
                     [else (error "A expressão espera valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro (Exp: MULTIPLICAÇÃO).")])]
      [(divv e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'int]
                     [else (error "A expressão espera valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro (Exp: DIVISÃO).")])]
       [(lt e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'boolean]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro. (Exp: MENOR QUE)")])]
       [(eeq e1 e2) (cond
                      [(and (eq? (check-value-or-variable ctx e1) 'int) (eq? (check-value-or-variable ctx e2) 'int) #t) 'boolean]
                     [else (error "A expressão espera dois valores inteiros, mas um dos ou os valores passados não correspondem a um inteiro. (Exp: É IGUAL)")])]
      [(eand e1 e2) (cond
                     [(and (eq? (check-value-or-variable ctx e1) 'boolean) (eq? (check-value-or-variable ctx e2) 'boolean) #t) 'boolean]
                     [else (error "A expressão espera dois valores boleanos, mas um dos ou os valores passados não correspondem a um boleano. (Exp: AND)")])]
      [(enot e1)    (cond
                     [(eq? (check-value-or-variable ctx e1) 'boolean) 'boolean]
                     [else (error "A expressão espera um valor booleano, porém o valor passado não corresponde a um boleano. (Exp: NOT)")])]))

(define (type-check-stmt ctx s)
  (match s
    [(assign t v e1)
       (let* ([expr (type-check-expr ctx e1)]
              [type-v (type-value t)]
              [variable (type-check-expr ctx v)])
             
                (if (eq? expr type-v)
                      (hash-set ctx variable type-v)
                      (error "A expressão não corresponde com o tipo da variável")))]
    [(eassign v e1)
      (let* ([expr (type-check-expr ctx e1)]
              [variable (type-check-expr ctx v)])ctx)]
    [(sprint e1)
       (let ([tl (type-check-expr ctx e1)])
             ctx)]
     [(read-v v)
       (let ([tl (type-check-expr ctx (evar v))])
             ctx)]
    [(eif econd then-block else-block)
        (let ([expr (type-check-expr ctx econd)])
             (if (eq? expr 'boolean)
                (begin
                  (type-check-stmts ctx then-block)
                  (type-check-stmts ctx else-block)
                  ctx)
                 (error "O comando espera uma expressão booleana, porém recebeu outro tipo de dados. (COM: IF)")))]
    [(ewhile econd block)
        (let ([expr (type-check-expr ctx econd)])
             (if (eq? expr 'boolean)
                 (type-check-stmts ctx block)
                 (error "O comando espera uma expressão booleana, porém recebeu outro tipo de dados. (COM: WHILE)")))]
    [(efor init e1 block)
        (let* ([ctx1 (type-check-stmt ctx init)]
               [expr (type-check-expr ctx e1)])
          (if (eq? expr 'int)
              (type-check-stmts ctx1 block)
              (error "O comando espera um numero inteiro, porém recebeu outro tipo de dados. (COM: FOR)")))]))

;ctx: contexto para tipos
(define (type-check-stmts ctx blk)
  (match blk
    ['() ctx]
    [(cons s blks) (let ([nctx (type-check-stmt ctx s)])
                      (begin
                       (type-check-stmts nctx blks)))]))

(define (type-check prog)
  (type-check-stmts (make-immutable-hash) prog))

(provide get-type type-check type-check-expr)