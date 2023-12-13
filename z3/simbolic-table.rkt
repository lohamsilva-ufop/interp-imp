#lang racket
(require "../syntax.rkt")

(struct record (inputs outputs) #:transparent)
(define table (make-hash))
(define temp-value (make-hash))

(define (insert-simbolic-table ev v)
  (hash-set!  table ev (list v)))

(define (update-simbolic-table ev v)
  (let ([old-value (hash-ref table ev)])
  (hash-set! table ev (cons v old-value))))

(define (show-table)
  (displayln table))

(define (show-record)
  (displayln record))

(define (consolidade-record-inputs)
 (record-inputs (record table null)))

(define (consolidade-record-outputs in out)
 (record-outputs (record in out)))

(provide (all-defined-out))
