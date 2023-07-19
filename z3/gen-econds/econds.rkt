#lang racket

(require "syntax.rkt"
         "type-check.rkt"
         "interp.rkt"
         "out/parser.rkt")
(require racket/format)

(define (create-assert-conditionals op e1 e2 list-assert)
  (cons (string-append "(assert (" op (~a e1) " , " (~a e2) "))") list-assert))

(define (verify-conditionals econd then-block else-block list-assert)
   (match econd
     [(lt e1 e2) (create-assert-conditionals ">" (evar-id e1) (value-value e2) list-assert)])
   list-assert)

(define (build-file-script econd then-block else-block)
  (let* ([string-assets-econd (verify-conditionals econd then-block else-block '())]
         [text-script (string append
                             string-assets-econd
                             "(check-sat) "
                             "(get-model)")]
         [script-file (create-script-file text-script)]
         [cmd (string-append "z3 " (path->string script-file))]
         [res (with-output-to-string (lambda () (system cmd)))])
         (displayln res)))
