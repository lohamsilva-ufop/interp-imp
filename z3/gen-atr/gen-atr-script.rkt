#lang racket

(require "../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt")

(require racket/date)

(define (build-str-assign v e1 str-assign ctx env)
  (string-append
   str-assign
   (string-append "(declare-const " (evar-id v) " " (string-titlecase(~a (get-type ctx v))) ") ")
  "(assert (= " (evar-id v) " " (~a (eval-expr env e1)) "))"))


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