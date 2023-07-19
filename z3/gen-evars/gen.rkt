#lang racket

(require "../out/syntax.rkt"
         "../type-check.rkt"
         "../interp.rkt"
         "../out/parser.rkt")
(require racket/format)

 (define (execute-gen n res text varlist)
   (if (= n 0)
       (displayln "Fisnish!")
       (begin
       (let ([res-new (get-tree-to-build-replace-script (parse (open-input-string res)) text varlist)])
             (execute-gen (- n 1) (cdr res-new) (car res-new) varlist)))))

(define (create-script-file text-script)
   (let*  ([path-file (string->path "script.z3")]
           [out (open-output-file path-file
                                  #:mode 'text
                                  #:exists 'replace)])
     (begin
      (displayln text-script out)
      (close-output-port out)
      path-file)))

(define (verify-node node list-tree)
  (match node
    [(define-const-vars e t v) (begin
                                 (let  ([new-list-tree (cons (cons e (cons t v)) list-tree)])
                                   new-list-tree))]
    [(sat-unsat v) list-tree]))

(define (build-string-variables string varlist)
  (if (equal? varlist '())
      string
  (let*  ([var  (car (car varlist))]
          [t    (car (cdr (car varlist)))]
          [new-string (string-append string "(declare-const " (evar-id var) " " (~a (type-value t)) ") " )])
                    (build-string-variables new-string (cdr varlist)))))

(define (build-string-assets string varlist)
  (if (equal? varlist '())
      string
  (let*  ([var  (car (car varlist))]
          [vl   (cdr (cdr (car varlist)))]
          [new-string (string-append string "(assert (not (= " (evar-id var) " " (~a (value-value vl))"))) ")])
                    (build-string-assets new-string (cdr varlist)))))

(define (replace-script str varlist)
  (let*  ([string-assets (build-string-assets "" varlist)]
          [text-script (string-append
                       (string-replace str "(check-sat) (get-model)" string-assets)
                       "(check-sat) "
                       "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           (displayln text-script)
           (displayln res)
           (cons text-script res))))

(define (build-script str varlist)
  (let*  ([string-assets (build-string-assets "" varlist)]
          [text-script (string-append
                       (string-replace str "(check-sat) (get-model)" string-assets)
                       "(check-sat) "
                       "(get-model)")]
          [script-file (create-script-file text-script)]
          [cmd (string-append "z3 " (path->string script-file))]
          [res (with-output-to-string (lambda () (system cmd)))])
           (begin
           ;(displayln text-script)
           ;(displayln res)
           (displayln (parse (open-input-string res))))
           (execute-gen 8 res text-script '())))

(define (get-tree-to-build-replace-script ast string list-tree)
(match ast
    ['() (replace-script string list-tree)]
    [(cons node res-ast) (begin
                       (let ([new-list-tree (verify-node node list-tree)])
                       (get-tree-to-build-replace-script res-ast string new-list-tree)))]))

                        
(define (get-tree-to-build-script script-text ast list-tree)
(match ast
    ['() (build-script script-text list-tree)]
    [(cons node res-ast) (begin
                       (let ([new-list-tree (verify-node node list-tree)])
                       (get-tree-to-build-script script-text res-ast new-list-tree)))]))

(provide get-tree-to-build-script get-tree-to-build-replace-script)