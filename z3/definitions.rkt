#lang racket

(require "syntax.rkt" "type-check.rkt" "interp.rkt")
(require racket/format)

(define (verify-node node ctx script)
  (match node
    [(read-v v1) (begin
                   (let*  ([type (get-type ctx v1)]
                          [script (string-append "(declare-const" (evar-id v1) (~a type) ")" )])
                     (display script)))]
    [(assign t v e1) (display "")]
     [(assign t v e1)(display "")]
    [(eassign v e1) (display "")]
    [(sprint e1) (display "")]
    [(read-v v1) (display "")]              
    [(eif econd then-block else-block) (display "")]
    [(ewhile econd block) (display "")]
    [(efor (assign t v ex) e1 block) (display "")]))
                        
(define (build-read-script ast ctx script)
(match ast
    ['() script]
    [(cons node astx) (begin
                       (verify-node node ctx script)
                       (build-read-script astx ctx script))]))

(define (get-read ctx prog)
  (build-read-script prog ctx ""))

(provide get-read)