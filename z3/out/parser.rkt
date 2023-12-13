#lang racket

(require parser-tools/yacc
         "lexer.rkt"
         "syntax.rkt")

(define imp-parser
  (parser
   (start statements)
   (end EOF)
   (tokens value-tokens var-tokens syntax-tokens)
   (src-pos)
   (error
    (lambda (a b c d e)
      (begin
        (printf "a = ~a\nb = ~a\nc = ~a\nd = ~a\ne = ~a\n"
                a b c d e) (void))))
   (grammar
    (statements [() '()]
                [(statement LPAREN statements RPAREN) (cons $1 $3)]
                [(statement statements) (cons $1 $2)])
    (statement [(LPAREN DEFINE-FUN expr LPAREN RPAREN type expr RPAREN) (define-const-vars $3 $6 $7)]
               [(SAT) (sat-unsat 'sat)]
               [(UNSAT) (sat-unsat 'unsat)])
    (expr  [(LPAREN SUBTRACT NUMBER RPAREN) (value (- $3))]
           [(NUMBER) (value $1)]
           [(IDENTIFIER) (evar $1)])
    (type [(INT) (type 'Int)]
          [(BOOLEAN) (type 'Boolean)]))))
(define (parse ip)
  (imp-parser (lambda () (next-token ip))))

(provide parse)
