#lang racket

(require "parser.rkt"
         "interp.rkt"
         "syntax.rkt"
          "../especificacao/table.rkt"
          "../especificacao/student-results.rkt")

(provide (rename-out [imp-spcf-read read]
                     [imp-spcf-read-syntax read-syntax]))

(define (imp-spcf-read in)
  (syntax->datum
   (imp-spcf-read-syntax #f in)))

(define (imp-spcf-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-mod racket
      ,(finish (imp-spcf-interp (parse port))))))

(define (finish env)
  ; (displayln "Tabela Resultante do gabarito: ")
  ; (displayln table)
  
  ;(displayln "Tabela Resultante do aluno: ")
   ;(displayln result-table)
  (displayln "Finished!")
  )