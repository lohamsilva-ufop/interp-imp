#lang racket

(require "parser.rkt"
         "controller.rkt"
         "syntax.rkt")

(provide (rename-out [imp-spcf-read read]
                     [imp-spcf-read-syntax read-syntax]))

(define (imp-spcf-read in)
  (syntax->datum
   (imp-spcf-read-syntax #f in)))

(define (imp-spcf-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-mod racket
      ,(finish (execution-controller (parse port))))))

(define (finish env)
  (displayln "Finished!"))