#lang racket

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUMBER))
(define-tokens var-tokens (IDENTIFIER))
(define-empty-tokens syntax-tokens
  (EOF
   ADD
   SUBTRACT
   PRODUCT
   DIVISION
   LT
   EQ
   ASSIGN
   NOT
   AND
   SEMI
   LPAREN
   RPAREN
   IF
   THEN
   ELSE
   WHILE
   FOR
   DO
   TO
   BEGIN
   END
   PRINT
   INPUT
   READ
   INT
   BOOLEAN
   TRUE
   FALSE))

(define next-token
  (lexer-src-pos
   [(eof) (token-EOF)]
   [(:+ #\newline whitespace)
    (return-without-pos (next-token input-port))]
   [#\+ (token-ADD)]
   [#\- (token-SUBTRACT)]
   [#\* (token-PRODUCT)]
   [#\/ (token-DIVISION)]
   [#\< (token-LT)]
   ["==" (token-EQ)]
   [":=" (token-ASSIGN)]
   ["!"  (token-NOT)]
   ["&&" (token-AND)]
   [";"  (token-SEMI)]
   ["("  (token-LPAREN)]
   [")"  (token-RPAREN)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["while" (token-WHILE)]
   ["for" (token-FOR)]
   ["do" (token-DO)]
   ["to" (token-TO)]
   ["begin" (token-BEGIN)]
   ["end" (token-END)]
   ["print" (token-PRINT)]
   ["input" (token-INPUT)]
   ["read" (token-READ)]
   ["int" (token-INT)]
   ["boolean" (token-BOOLEAN)]
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   [(:: alphabetic (:* (:+ alphabetic numeric)))
    (token-IDENTIFIER lexeme)]
   [(:: numeric (:* numeric))
    (token-NUMBER (string->number lexeme))]))

(provide next-token value-tokens var-tokens syntax-tokens)
