#lang racket

(struct define-const-vars
  (evar type value)
  #:transparent)

(struct sat-unsat
  (value)
  #:transparent)

(struct value
  (value)
  #:transparent)

(struct evar
  (id)
  #:transparent)

(struct type
  (value)
  #:transparent)


(provide (all-defined-out))
