#lang racket
(require "../../syntax.rkt"
         "../../interp.rkt"
         "../gen-evars/gen.rkt"
         "definitions.rkt"
          "../gen-atr/gen-atr-script.rkt")

(define g (tree-econds (lt (evar "numero") (value 3))
(tree-econds (lt (evar "multiplo") (value 4))
(tree-econds (lt (evar "valor") (value 5))
(tree-econds (lt (evar "dividendo") (value 6)) '() '()) '()) '())
'()))

(define (eval-expr-gen-econds-test e)
  (match e
   [(evar e1) (~a e1)]
   [(value val) (~a val)]))


(define (check-econd-test node)
  (match node
    [(lt e1 e2)   (string-append "(< " (~a (eval-expr-gen-econds-test e1)) "  " (~a (eval-expr-gen-econds-test e2)) ")")]))

(define (gen-econd-block-test tree str-cond)
  (displayln tree)
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let*
                            ([new-str-x (string-append str-cond (check-econd-test x))]
                             [new-str-y (gen-econds-node-test y new-str-x)]
                             [new-str-z (gen-econds-node-test z new-str-y)])
                              (displayln new-str-z))]))

(define (gen-econds-node-test tree-econd str-cond)
  (match tree-econd
    ['() str-cond]
    ['null str-cond]
    [(cons f rest) (let([new (gen-econd-block-test f str-cond)])
                      (gen-econds-node-test rest new))]))
 
(define (gen-script-eifs-test node)
  (match node
    ['() '()]
    [ (tree-econds x y z) (gen-econd-block-test y "")]))

(gen-script-eifs-test g)
;(displayln g)