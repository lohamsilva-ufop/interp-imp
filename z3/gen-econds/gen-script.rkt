#lang racket

(require "../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt"
         "../out/parser.rkt"
         "../gen-evars/gen.rkt"
         "definitions.rkt")

(require racket/date)

(define (create-script-file text-script)
   (let*  ([path-file (string->path (string-append "../z3/scripts/econd/script_" (~a (random (date->seconds (current-date)))) ".z3"))]
           [out (open-output-file path-file
                                  #:mode 'text
                                  #:exists 'replace)])
     (begin
      (displayln text-script out)
      (close-output-port out)
      path-file)))

(define (gen-script text-script)
  (begin
  (let*  ([script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           ;(displayln res)
           ))))

(define (split-econds str)
  (let*
      ([init (substring str 0 9)]
       [end  (string-replace str init "")])
    (cons init end)))

(define (check-variables node ctx)
   (match node
         [(lt e1 e2) (let* ([type (get-type ctx e1)]
                            [varx (evar-id e1)])
                       (cons varx type))]))

(define (check-econd node)
  (match node
   [(lt e1 e2)   (string-append "(< " (evar-id e1) "  " (~a (value-value e2)) ")")]))

(define (gen-str-variables node ctx)
  (let*
      ([res (check-variables (car node) ctx)]
       [evar (car res)]
       [type (cdr res)])
  (string-append "(declare-const " evar " " (string-titlecase (~a type)) ") " )))
                           

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
    ['() str-cond]
    [(cons f rest) (let([ new (gen-script-else-block f str-cond)])
                      (gen-script-eifs-else rest new))]))

(define (execute-gen-script-econds florest str-cond ctx)
  (let
    ([str-then (gen-script-eifs-then florest str-cond)])
    (if (equal? 9 (string-length str-then))
        (let* 
        ([str-text-script-then
           (string-append
           ;(gen-str-variables florest ctx)
           "(declare-const x Int)"
          " (assert " str-then ")"
          "(check-sat) "
         "(get-model)")]

          [str-text-script-else
           (string-append
           ;(gen-str-variables florest ctx)
           "(declare-const x Int)"
          " (assert (not " str-then "))"
          "(check-sat) "
         "(get-model)")])

          (begin
            (gen-script str-text-script-then)
            (gen-script str-text-script-else)))
        (gen-script-eifs-more-then-one str-then "" florest))))
        
        

(define (gen-script-eifs-more-then-one str-then str-cond florest)
   (let*
    ([init-then (car (split-econds str-then))]
     [end-then  (cdr (split-econds str-then))]
     [str-else (gen-script-eifs-else florest str-cond)]
     [init-else (car (split-econds str-else))]
     [end-else (cdr (split-econds str-else))]
     
     [str-text-script-then-true
      (string-append
        ;(gen-str-variables florest ctx)
       "(declare-const x Int)"
        " (assert (and " str-then "))"
        "(check-sat) "
        "(get-model)")]

     [str-text-script-then-false
      (string-append
        ;(gen-str-variables florest ctx)
       "(declare-const x Int)"
        " (assert (and " init-then "  (not " end-then ")))"
        "(check-sat) "
        "(get-model)")]

     
     [str-text-script-else-true
      (string-append
       ;(gen-str-variables florest ctx)
       "(declare-const x Int)"
        " (assert (and (not " init-else ") " end-else "))"
        "(check-sat) "
        "(get-model)")]

     [str-text-script-else-false
      (string-append
       ;(gen-str-variables florest ctx)
       "(declare-const x Int)"
        " (assert (and (not " init-else ") (not " end-else ")))"
        "(check-sat) "
        "(get-model)")])
    (begin
    (gen-script str-text-script-then-true)
    (gen-script str-text-script-then-false)
    (gen-script str-text-script-else-true)
    (gen-script str-text-script-else-false))))


(provide (all-defined-out))