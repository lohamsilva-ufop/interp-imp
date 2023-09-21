#lang racket

(require "../../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt")

(require racket/date)

(define (eval-expr-gen-atr e)
  (match e
   [(add e1 e2)  (string-append "( + " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(minus e1 e2)(string-append "( - " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(mult e1 e2) (string-append "( * " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(divv e1 e2) (string-append "( / " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lt e1 e2)   (string-append "( > " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bt e1 e2)   (string-append "( < " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lte e1 e2)  (string-append "( <= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bte e1 e2)  (string-append "( >= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eeq e1 e2)  (string-append "( = " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eand e1 e2) (string-append "( and " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eor e1 e2)  (string-append "( or " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(enot e1) (string-append "( not " (eval-expr-gen-atr e1) ")")]
   [(evar e1) (~a (evar e1))]
   [(value val) (~a val)]))
  

(define (build-str-assign v e1 str-assign ctx env)
  (string-append
   str-assign
   (string-append "(declare-const " (evar-id v) " " (string-titlecase(~a (get-type ctx v))) ") ")
  "(assert (= " (evar-id v) " " (eval-expr-gen-atr e1)))


(define (get-assign ast str-assign ctx env)
   (match ast
    ['() str-assign]
    [(cons (read-v v1) astrest) (get-assign astrest str-assign ctx env)]
    [(cons (assign t v e1) astrest) (get-assign astrest (build-str-assign v e1 str-assign ctx env) ctx env)]
    [(cons (sprint e1) astrest) (get-assign astrest str-assign ctx env)]
    [(cons (read-v v1) astrest) (get-assign astrest str-assign ctx env)]              
    [(cons (eif econd then-block else-block) astrest)

     (begin
                                                        (let*
                                                        ([str-then (get-assign then-block "" ctx env)]
                                                        [str-else (get-assign else-block "" ctx env)]
                                                         [str-assign-efor (string-append str-assign str-then str-else)])
                                                        
                                                       (get-assign astrest str-assign-efor ctx env)))]
                                                      ; (get-assign astrest str-assign ctx env)]
    [(cons (ewhile econd block) astrest) (get-assign astrest str-assign ctx env)]
    [(cons (efor (assign t v ex) e1 block) astrest) (get-assign astrest str-assign ctx env)]))


(provide (all-defined-out))