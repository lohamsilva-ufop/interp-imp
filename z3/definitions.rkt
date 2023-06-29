#lang racket

(require "syntax.rkt" "type-check.rkt" "interp.rkt")
(require racket/format)

(define (create-script-file text-script)
   (let*  ([path-file (make-temporary-file "script-~a.z3")]
           [out (open-output-file path-file
                                  #:mode 'text
                                  #:exists 'truncate)])
     (begin
      (displayln text-script out)
      (close-output-port out)
      path-file)))

(define (verify-node node ctx varlist)
  (match node
    [(read-v v1) (begin
                   (let*  ([type (get-type ctx v1)]
                          [new-varlist (cons (cons (evar-id v1) (string-titlecase(~a type))) varlist)])
                     new-varlist))]
    [(assign t v e1) varlist]
    [(sprint e1) varlist]
    [(read-v v1) varlist]              
    [(eif econd then-block else-block) varlist]
    [(ewhile econd block) varlist]
    [(efor (assign t v ex) e1 block) varlist]))

(define (build-string-variables string varlist)
  (if (equal? varlist '())
      string
  (let*  ([var  (car (car varlist))]
          [type (cdr (car varlist))]
          [new-string (string-append string "(declare-const " var " " type ") " )])
                    (build-string-variables new-string (cdr varlist)))))

(define (build-string-assets string varlist)
  (if (equal? varlist '())
      string
  (let*  ([var (car (car varlist))]
          [new-string (string-append string "(assert (> " var " 0)) ")])
                    (build-string-assets new-string (cdr varlist)))))

(define (build-script string varlist)
  (let*  ([string-varlist (build-string-variables string varlist)]
          [string-assets (build-string-assets string varlist)]
          [text-script (string-append
                      string-varlist
                      string-assets
                      "(check-sat) "
                      "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res))))

                        
(define (build-read-script ast ctx varlist)
(match ast
    ['() (build-script "" varlist)]
    [(cons node astx) (begin
                       (let ([new-varlist (verify-node node ctx varlist)])
                       (build-read-script astx ctx new-varlist)))]))

(define (get-read ctx prog)
  (build-read-script prog ctx '()))

(provide get-read)