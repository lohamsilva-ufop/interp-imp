#lang racket

(require "parser.rkt"
         "interp.rkt"
         "syntax.rkt"
         "type-check.rkt"
         "definitions.rkt")

(provide (rename-out [imp-read read]
                     [imp-read-syntax read-syntax]))

(define (imp-read in)
  (syntax->datum
   (imp-read-syntax #f in)))

(define (imp-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-mod racket
      ,(let ([ast (parse port)])
      (displayln (type-check ast))
      (imp-interp ast)))))
      ;(get-read (type-check ast) ast)))))

(define (finish env)
  (displayln "Finished!"))