#lang racket

(require "parser.rkt"
         "interp.rkt"
         "syntax.rkt"
         "type-check.rkt"
         "./gen-evars/definitions.rkt"
         "./gen-evars/gen.rkt"
         "./gen-econds/gen-script.rkt"
         "./gen-econds/definitions.rkt")

(provide (rename-out [imp-read read]
                     [imp-read-syntax read-syntax]))


(define (imp-read in)
  (syntax->datum
   (imp-read-syntax #f in)))

(define (imp-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-mod racket
      ,(let* ([ast (parse port)]
              [ctx (type-check ast)])
              ;[scz3 (get-read ctx ast)])
         ;(build-ifs-script ast ctx)))))
          (execute-gen-script-econds (get-eifs ast) "" ctx)))))
    

(define (finish env)
  (displayln "Finished!"))