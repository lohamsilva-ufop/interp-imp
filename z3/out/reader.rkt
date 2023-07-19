#lang racket

(require "parser.rkt"
         "interp.rkt"
         "syntax.rkt")

(provide (rename-out [outz3-read read]
                     [outz3-read-syntax read-syntax]))

(define (outz3-read in)
  (syntax->datum
   (outz3-read-syntax #f in)))

(define (outz3-read-syntax path port)
  (datum->syntax
   #f
   `(module outz3-mod racket
      ,(outz3-interp (parse port)))))

(define (finish env)
  (displayln "Finished!"))