#lang racket

(require "syntax.rkt"
         "../parser.rkt"
         "../temp-interp.rkt"
         "../z3/gen-econds/gen-script.rkt"
         "../z3/gen-econds/definitions.rkt")

(define (return-tree-syntax path)
  (let* ([port  (open-input-file path)]
                [text  (string-replace (read-string 1000 port) "#lang interp-imp/python/z3/testz3" "")]
                [ast (parse (open-input-string text))])
    (parse (open-input-string text))))

(define (executa-gabarito path)
        (let  ([ast  (return-tree-syntax path)])
               (execute-gen-script-econds ast (get-eifs ast) "")))

(define (control-execute-gab numero-execucoes path-gabarito)
  (let*
      ([table (executa-gabarito path-gabarito)]
       [ast  (return-tree-syntax path-gabarito)]
       [copy-table table])
      (temp-python-interp ast numero-execucoes copy-table table '())))

(define (execution-controller cfg)
  (match cfg
   [(config numero-execucoes gabarito dir-aluno-exercicios)
       (begin
       (control-execute-gab (value-value numero-execucoes) (value-value gabarito)))]))
       ;(percorre-studentes-path (directory-list (value-value dir-aluno-exercicios))(value-value dir-aluno-exercicios) (value-value numero-execucoes)))]))

(provide execution-controller)