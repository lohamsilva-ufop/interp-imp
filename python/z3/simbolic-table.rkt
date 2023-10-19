#lang racket
(require "syntax.rkt")

(struct record (inputs outputs) #:transparent)
(define table (make-hash))

(define (insert-simbolic-table ev v)
  (hash-set!  table ev v))

(define (update-simbolic-table ev v)
  (let ([old-value (hash-ref table ev)])
  (hash-set! table ev (cons old-value v))))

(define (show-table)
  (displayln table))

(provide (all-defined-out))
