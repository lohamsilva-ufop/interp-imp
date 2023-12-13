#lang racket

(require "../../syntax.rkt"
         "../../interp.rkt"
         "definitions.rkt"
         "gen-text-econds.rkt")


(define (check-econd node)
  (match node
    [(lt e1 e2)   (string-append "(< " (evar-id e1) "  " (~a (value-value e2)) ")")]
    [(bt e1 e2)   (string-append "(> " (evar-id e1) "  " (~a (value-value e2)) ")")]
    [(lte e1 e2)   (string-append "(<= " (evar-id e1) "  " (~a (value-value e2)) ")")]
    [(bte e1 e2)   (string-append "(>= " (evar-id e1) "  " (~a (value-value e2)) ")")]
    [(eeq e1 e2)   (string-append "(= " (evar-id e1) "  " (~a (value-value e2)) ")")]))
                           

(define (gen-script-true-block tree str-cond)
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let
                            ([new-str-cond (string-append str-cond (check-econd x))])
                            (begin
                            (gen-script-eifs-then y new-str-cond)))]))
                            

(define (gen-script-else-block tree str-cond)
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let
                            ([new-str-cond (string-append str-cond (check-econd x))])
                            (begin
                            (gen-script-eifs-else z new-str-cond)))]))
   
(define (gen-script-eifs-then florest str-cond)
  (match florest
    ['() str-cond]
    [(cons f rest) (let([ new (gen-script-true-block f str-cond)])
                      (gen-script-eifs-then rest new))]))

(define (gen-script-eifs-else florest str-cond)
  (match florest
    ['null str-cond]
    ['() str-cond]
    [(cons f rest) (let([ new (gen-script-else-block f str-cond)])
                      (gen-script-eifs-else rest new))]))


(define (gen-script-eifs node ast )
  (match node
    ['() '()]
    [ (tree-econds x y z) (gen-text x y z ast )]))
  
(define (execute-gen-script-econds ast florest str )
  (match florest
    ['() str]
    [(cons node rest) (gen-script-eifs node ast )]))

(provide (all-defined-out))