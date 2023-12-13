#lang racket

(require parser-tools/yacc
         "lexer.rkt"
         "syntax.rkt")

(define spcf-parser
  (parser
   (start statement)
   (end EOF)
   (tokens value-tokens var-tokens syntax-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin
        (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n"
                a b c d e) (void))))
  
   (grammar
    (statement
     [(QTEXEC COLON expr SEMI
       GABARITO COLON expr SEMI
       EXERCICIOS COLON expr SEMI) (config $3 $7 $11)])
    (expr  [(NUMBER) (value $1)]
           [(STRING) (value $1)]))))

(define (parse ip)
  (spcf-parser (lambda () (next-token ip))))

(provide parse)
