#lang racket

(require "syntax.rkt"
         "../python/parse-python/lex+yacc.rkt"
         "../python/temp-interp.rkt"
         "../z3/gen-econds/gen-script.rkt"
         "../z3/gen-econds/definitions.rkt")

#;(define (return-tree-syntax-old path)
  (let* ([port  (open-input-file path)]
                [text  (string-replace (read-string 1000 port) "#lang interp-imp/python/z3/testz3" "")]
                [ast (parse (open-input-string text))])
    (parse (open-input-string text))))

(define (return-tree-syntax path)
  (build-ast-from-file path))

(define (correction list-out-gabarito list-out-ex-alunos)
  (if (equal? list-out-gabarito list-out-ex-alunos)
      (displayln "O exercício está correto. ")
      (displayln "O exercício está incorreto. ")))

(define (percorre-path-aluno path list-files-path nexc table-inputs list-outs-gab)
  (match list-files-path
    ['() (displayln "Correção Finalizada. ")]
    [(cons f rest)
       (let* ([path-file (string-append path "/" (~a f))]
             [pair-ex-student (control-execute-students nexc path-file table-inputs)])
       (begin
        (display "Arquivo: ")
        (displayln f)
        (displayln "Saídas do gabarito: ")
        (displayln list-outs-gab)
        (displayln "Saídas após a execução do aluno com as entradas do gabarito: ")
        (displayln (cdr pair-ex-student))
        (display "Correção: ")
        (correction list-outs-gab (cdr pair-ex-student))
        (displayln "===========================================")
        (percorre-path-aluno path rest nexc (car pair-ex-student) list-outs-gab)))]))

(define (executa-gabarito path)
        (let  ([ast  (return-tree-syntax path)])
               (execute-gen-script-econds ast (get-eifs ast) "")))

(define (control-execute-students numero-execucoes path-exercise table-inputs)
  (let ([ast  (return-tree-syntax path-exercise)])
      (temp-python-interp ast numero-execucoes table-inputs '())))

(define (control-execute-gab numero-execucoes path-gabarito)
  (let*
      ([table (executa-gabarito path-gabarito)]
       [ast  (return-tree-syntax path-gabarito)])
      (temp-python-interp ast numero-execucoes table '())))

(define (execution-controller cfg)
  (match cfg
   [(config numero-execucoes gabarito dir-aluno-exercicios)
       (let*
       ([nexec (value-value numero-execucoes)]
        [path-gab (value-value gabarito)]
        [path-alunos (value-value dir-aluno-exercicios)]
        [pair-gab (control-execute-gab nexec path-gab)]
        [table-inputs-gab (car pair-gab)]
        [list-outs-gab (cdr pair-gab)]
        [list-files-path (directory-list path-alunos)])
        (percorre-path-aluno path-alunos list-files-path nexec table-inputs-gab list-outs-gab))]))
       
(provide execution-controller)