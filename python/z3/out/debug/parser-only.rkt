#lang racket


(module reader racket
  (require "../parser.rkt")
  (provide (rename-out [imp-read read]
                       [imp-read-syntax read-syntax]))


  (define (imp-read in)
    (syntax->datum
     (imp-read-syntax #f in)))

  (define (imp-read-syntax path port)
    (datum->syntax
     #f
     `(module imp-mod racket
        ,@(parse port)))))
