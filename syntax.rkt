#lang racket

;; expression syntax

(struct input
  (id)
  #:transparent)

(struct value
  (value)
  #:transparent)

(struct type
  (value)
  #:transparent)

(struct evar
  (id)
  #:transparent)

(struct add
  (left right)
  #:transparent)

(struct minus
  (left right)
  #:transparent)

(struct mult
  (left right)
  #:transparent)

(struct divv
  (left right)
  #:transparent)

(struct eeq
  (left right)
  #:transparent)

(struct lt
  (left right)
  #:transparent)

(struct bt
  (left right)
  #:transparent)

(struct lte
  (left right)
  #:transparent)

(struct bte
  (left right)
  #:transparent)

(struct diff
  (left right)
  #:transparent)

(struct eand
  (left right)
  #:transparent)

(struct eor
   (left right)
  #:transparent)


(struct enot
  (arg)
  #:transparent)

; statement syntax

(struct assign
  (type var expr)
  #:transparent)

(struct eassign
  (var expr)
  #:transparent)

(struct sprint
  (expr)
  #:transparent)

(struct read-v
  (evar)
  #:transparent)

(struct eif
  (econd then-block else-block)
  #:transparent)

(struct eif-only-then
  (econd then-block)
  #:transparent)

(struct ewhile
  (econd block)
  #:transparent)

(struct efor
  (init expr block)
  #:transparent)

(provide (all-defined-out))
