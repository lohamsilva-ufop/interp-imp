#lang racket
(require "../../syntax.rkt"
         "../../interp.rkt"
         "../gen-evars/gen.rkt"
         "definitions.rkt"
          "../gen-atr/gen-atr-script.rkt")

(require racket/date)

(define (create-script-file text-script)
   (let*  ([path-file (string->path (string-append "../../python/z3/scripts/econd/script_" (~a (random (date->seconds (current-date)))) ".z3"))]
           [out (open-output-file path-file
                                  #:mode 'text
                                  #:exists 'replace)])
     (begin
      (displayln text-script out)
      (close-output-port out)
      path-file)))

(define (execute-script text-script)
  (begin
  (let*  ([script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
         (displayln res)))))

(define (build-text-script str)
 (execute-script (string-append
     str
     "(check-sat) "
     "(get-model)")))

(define (eval-expr-gen-econds e)
  (match e
   [(add e1 e2)  (string-append "( + " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(minus e1 e2)(string-append "( - " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(mult e1 e2) (string-append "( * " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(divv e1 e2) (string-append "( / " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(esqrt e1)   (string-append "(Sqrt (" (eval-expr-gen-atr e1) ")")]
   [(mod e1 e2)  (string-append "( mod " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lt e1 e2)   (string-append "( < " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bt e1 e2)   (string-append "( > " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(lte e1 e2)  (string-append "( <= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(bte e1 e2)  (string-append "( >= " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eeq e1 e2)  (string-append "( = " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eand e1 e2) (string-append "( and " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(eor e1 e2)  (string-append "( or " (eval-expr-gen-atr e1) " " (eval-expr-gen-atr e2) ")")]
   [(enot e1) (string-append "( not " (eval-expr-gen-atr e1) ")")]
   [(evar e1) (~a e1)]
   [(value val) (~a val)]))


(define (check-econd node env)
  (match node
    [(lt e1 e2)   (string-append "(< " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(bt e1 e2)   (string-append "(> " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(lte e1 e2)   (string-append "(<= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(bte e1 e2)   (string-append "(>= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eeq e1 e2)   (string-append "(= " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eand e1 e2)   (string-append "(and " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(eor e1 e2)   (string-append "(or " (~a (eval-expr-gen-econds e1)) "  " (~a (eval-expr-gen-econds e2)) ")")]
    [(enot e1)    (string-append "(not " (~a (eval-expr-gen-econds e1))")")]))


(define (gen-econd-block-false tree str-cond env)
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let*
                            ([new-str-x (string-append str-cond "(not " (check-econd x env) ") ")]
                             [new-str-y (gen-econds-node-false y new-str-x env)]
                             [new-str-z (gen-econds-node-false z new-str-y env)])
                              new-str-z)]))

(define (gen-econds-node-false tree-econd str-cond env)
  (match tree-econd
    ['() str-cond]
    ['null str-cond]
    [(cons f rest) (let([new (gen-econd-block-false f str-cond env)])
                      (gen-econds-node-false rest new env))]))

(define (gen-econd-block tree str-cond env)
  (match tree
    ['() '()]
    [ (tree-econds x y z) (let*
                            ([new-str-x (string-append str-cond (check-econd x env))]
                             [new-str-y (gen-econds-node y new-str-x env)]
                             [new-str-z (gen-econds-node z new-str-y env)])
                              new-str-z)]))

(define (gen-econds-node tree-econd str-cond env)
  (match tree-econd
    ['() str-cond]
    ['null str-cond]
    [(cons f rest) (let([new (gen-econd-block f str-cond env)])
                      (gen-econds-node rest new env))]))


(define (text-x-y-z x y z ast env)
  (let*
    ([node-x (check-econd x env)]
     [node-y (gen-econds-node y "" env)]
     [node-z (gen-econds-node z "" env)]
     [str-assign (get-assign ast "" env)]
     [str-script-then-true (string-append
                         str-assign
                        " (assert (and " node-x " " node-y "))")]
     [str-script-then-false (string-append
                         str-assign
                        " (assert (and " node-x " " (gen-econds-node-false y "" env) "))")]
     [str-script-else-true (string-append
                         str-assign
                        " (assert (and (not " node-x ") " node-z "))")]
     [str-script-else-false (string-append
                         str-assign
                        " (assert (and (not " node-x ") " (gen-econds-node-false z "" env)  "))")])
     (begin
     (build-text-script str-script-then-true)
     (build-text-script str-script-then-false)
     (build-text-script str-script-else-true)
     (build-text-script str-script-else-false))))



(define (text-only-x-z x z ast env)
  (let*
    ([node-x (check-econd x env)]
     [node-z (gen-econds-node z "" env)]
     [str-assign (get-assign ast "" env)]
     [str-script-true (string-append
                         str-assign
                        " (assert " node-x ")")]
     [str-script-true-z (string-append
                         str-assign
                        " (assert (and (not " node-x ") " node-z "))")]
     [str-script-false-z (string-append
                         str-assign
                        " (assert (and (not " node-x ") " (gen-econds-node-false z "" env)  "))")])
     (begin
     (build-text-script str-script-true)
     (build-text-script str-script-true-z)
     (build-text-script str-script-false-z))))

(define (text-only-x-y x y ast env)
  (let*
    ([node-x (check-econd x env)]
     [node-y (gen-econds-node y "" env)]
     [str-assign (get-assign ast "" env)]
     [str-script-true (string-append
                         str-assign
                        " (assert (and " node-x " " node-y "))")]
     [str-script-false (string-append
                         str-assign
                        " (assert (not " node-x "))")])
     (begin
     (build-text-script str-script-true)
     (build-text-script str-script-false))))


(define (text-only-x x ast env)
  
(let*
    ([node (check-econd x env)]
     [str-assign (get-assign ast "" env)]
     [str-script-true (string-append
                         str-assign
                        " (assert " node ")")]
     [str-script-false (string-append
                         str-assign
                        " (assert (not " node "))")])
     (begin
     (build-text-script str-script-true)
     (build-text-script str-script-false))))
  

(define (gen-text x y z ast env)
  (cond
    [(and (equal? 0 (length y)) (or (equal? z 'null) (equal? 0 (length z))))  (text-only-x x ast env)]
    [(and (> (length y) 0) (or (equal? z 'null) (equal? 0 (length z))))  (text-only-x-y x y ast env)]
    [(and (or (equal? (length y) 0) (equal? y 'null)) (> (length z) 0))  (text-only-x-z x z ast env)]
    [(and (> (length y) 0) (> (length z) 0))  (text-x-y-z x y z ast env)]))

(provide gen-text)