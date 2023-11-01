#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER STRING))
(define-tokens var-tokens (IDENTIFIER))
(define-empty-tokens syntax-tokens
  (EOF
   QTEXEC
   COLON
   GABARITO
   EXERCICIOS
   SEMI
   COMMA
   QLINHA))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ #\newline whitespace)
    (return-without-pos (next-token input-port))]
   ["quantidade-execucoes" (token-QTEXEC)]
   [":" (token-COLON)]
   ["gabarito"  (token-GABARITO)]
   ["dir-aluno-exercicios" (token-EXERCICIOS)]
   [";"  (token-SEMI)]
   ["\n"  (token-QLINHA)]
   [(:seq #\" (complement (:seq any-string #\" any-string)) #\")
    (token-STRING (substring lexeme 1 (sub1 (string-length lexeme))))]
   [(:: alphabetic (:* (:+ alphabetic numeric)))
    (token-IDENTIFIER lexeme)]
   [(:: numeric (:* numeric))
    (token-NUMBER (string->number lexeme))]))

(provide next-token value-tokens var-tokens syntax-tokens)