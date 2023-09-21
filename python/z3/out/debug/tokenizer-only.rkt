#lang racket

(require "../lexer.rkt"
         syntax/strip-context
         parser-tools/lex)

(define (imp-lex-test ip)
  (port-count-lines! ip)
  (letrec ([one-line
            (lambda ()
              (let ([result (next-token ip)])
                (unless (equal?	(position-token-token result) 'EOF)
                  (printf "~a\n" result)
                  (one-line)
                  )))])
    (one-line)))


(define (imp-read in)
  (syntax->datum
    (imp-read-syntax #f in)))

(define (imp-read-syntax path port)
  (datum->syntax
   #f
   `(module imp-tokenize-mod racket
      ,(imp-lex-test port))))

(module+ reader (provide (rename-out [imp-read read]
                                     [imp-read-syntax read-syntax])))
