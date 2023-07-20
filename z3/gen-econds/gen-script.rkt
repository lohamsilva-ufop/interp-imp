#lang racket

(require "../syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt"
         "../out/parser.rkt"
         "../gen-evars/gen.rkt")
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

(define (build-string-variables string varlist)
  (if (equal? varlist '())
      string
  (let*  ([var  (car (car varlist))]
          [type (cdr (car varlist))]
          [new-string (string-append string "(declare-const " var " " (string-titlecase (~a type)) ") " )])
                    (build-string-variables new-string (cdr varlist)))))


(define (build-string-asserts-then string econd condlist)
  (if (equal? condlist '())
      string
  (let*  ([cond-item-list (car condlist)]
          [new-string (string-append string "(assert (and " econd  " " cond-item-list "))")])
                    (build-string-asserts-then new-string econd (cdr condlist)))))

(define (build-string-asserts-else string econd condlist)
  (if (equal? condlist '())
      string
  (let*  ([cond-item-list (car condlist)]
          [new-string (string-append string "(assert (and " econd  " (not " cond-item-list ")))")])
                    (build-string-asserts-else new-string econd (cdr condlist)))))

(define (build-string-asserts-else-true string econd condlist)
  (if (equal? condlist '())
      string
  (let*  ([cond-item-list (car condlist)]
          [new-string (string-append string "(assert (and (not" econd  ") " cond-item-list "))")])
                    (build-string-asserts-else new-string econd (cdr condlist)))))

(define (build-string-asserts-else-false string econd condlist)
  (if (equal? condlist '())
      string
  (let*  ([cond-item-list (car condlist)]
          [new-string (string-append string "(assert (and (not" econd  ") (not " cond-item-list ")))")])
                    (build-string-asserts-else new-string econd (cdr condlist)))))


(define (gen-script-then-block-true econd list-then-block varlist)
  (begin
  (let*  ([string-vars (build-string-variables "" varlist)]
          [string-asserts (build-string-asserts-then "" econd list-then-block)]
          [text-script (string-append
                      string-vars
                      string-asserts
                      "(check-sat) "
                      "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)))))

(define (gen-script-then-block-false econd list-then-block varlist)
  (begin
  (let*  ([string-vars (build-string-variables "" varlist)]
          [string-asserts (build-string-asserts-else "" econd list-then-block)]
          [text-script (string-append
                      string-vars
                      string-asserts
                      "(check-sat) "
                      "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)))))

(define (gen-script-else-block-true econd list-then-block varlist)
  (begin
  (let*  ([string-vars (build-string-variables "" varlist)]
          [string-asserts (build-string-asserts-else-true "" econd list-then-block)]
          [text-script (string-append
                      string-vars
                      string-asserts
                      "(check-sat) "
                      "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)))))

(define (gen-script-else-block-false econd list-then-block varlist)
  (begin
  (let*  ([string-vars (build-string-variables "" varlist)]
          [string-asserts (build-string-asserts-else-false "" econd list-then-block)]
          [text-script (string-append
                      string-vars
                      string-asserts
                      "(check-sat) "
                      "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)))))


(provide (all-defined-out))