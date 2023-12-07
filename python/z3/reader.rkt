#lang racket

(require "../parser.rkt"
         "./gen-econds/gen-script.rkt"
         "./gen-econds/definitions.rkt"
         "../../parse-python/lex+yacc.rkt")

(provide (rename-out [imp-read read]
                     [imp-read-syntax read-syntax]))


(define (imp-read in)
  (syntax->datum
   (imp-read-syntax #f in)))

(define (imp-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-mod racket
      ,(let* ([ast (parse port)])
          (execute-gen-script-econds ast (get-eifs ast) "")))))
    

(define (finish env)
  (displayln "Finished!"))